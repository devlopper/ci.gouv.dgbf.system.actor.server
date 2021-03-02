package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.ByteArrayOutputStream;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.enterprise.context.ApplicationScoped;
import javax.persistence.EntityManager;
import javax.transaction.Transactional;

import org.cyk.utility.business.server.EntitySaver;
import org.cyk.utility.business.server.EntitySaver.Arguments;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.configuration.ConfigurationHelper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.file.FileType;
import org.cyk.utility.__kernel__.log.LogHelper;
import org.cyk.utility.__kernel__.map.MapHelper;
import org.cyk.utility.__kernel__.number.NumberHelper;
import org.cyk.utility.__kernel__.object.__static__.persistence.EntityLifeCycleListener;
import org.cyk.utility.__kernel__.object.marker.IdentifiableSystem;
import org.cyk.utility.persistence.EntityManagerGetter;
import org.cyk.utility.persistence.query.EntityFinder;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.random.RandomHelper;
import org.cyk.utility.__kernel__.rest.RestHelper;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.throwable.ThrowableHelper;
import org.cyk.utility.__kernel__.throwable.ThrowablesMessages;
import org.cyk.utility.__kernel__.value.ValueHelper;
import org.cyk.utility.mail.MailSender;
import org.cyk.utility.report.ReportGetter;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.RequestBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.RequestPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.IdentificationFormQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentificationAttribute;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestStatus;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.impl.FreeMarker;

@ApplicationScoped
public class RequestBusinessImpl extends AbstractBusinessEntityImpl<Request, RequestPersistence> implements RequestBusiness,Serializable {
	private static final long serialVersionUID = 1L;
	
	private static void validate(Request request,Boolean budgetariesScopeFunctionsIncludable) {
		if(request == null)
			throw new RuntimeException("La demande est obligatoire");
		if(request.getType() == null)
			throw new RuntimeException("Le type de demande est obligatoire");
		if(request.getType().getForm() == null)
			throw new RuntimeException("Le formulaire de demande est obligatoire");
		if(Boolean.TRUE.equals(request.getAuthenticationRequired())) {
			if(request.getActor() == null)
				throw new RuntimeException("L'acteur qui demande est obligatoire");
		}else {
			if(StringHelper.isBlank(request.getElectronicMailAddress()))
				throw new RuntimeException("Le mail du demandeur est obligatoire");
		}
		if(Boolean.TRUE.equals(budgetariesScopeFunctionsIncludable) && RequestType.CODE_DEMANDE_POSTES_BUDGETAIRES.equals(request.getType().getCode())) {
			if(CollectionHelper.isEmpty(request.getBudgetariesScopeFunctions()))
				throw new RuntimeException("La fonction budgétaire est obligatoire");
			for(ScopeFunction budgetaryScopeFunction : request.getBudgetariesScopeFunctions()) {
				if(Boolean.TRUE.equals(budgetaryScopeFunction.getFunction().isCodeBelongsToExecutionHoldersCodes())) {
					if(NumberHelper.isGreaterThanZero(RequestScopeFunctionQuerier.getInstance()
							.countWhereGrantedIsTrueByScopeFunctionsIdentifiers(List.of(budgetaryScopeFunction.getIdentifier()))))
						throw new RuntimeException(String.format("La fonction budgétaire %s n'est pas disponible", budgetaryScopeFunction.toString()));
				}				
			}
		}
	}
	
	private void validateRecord(Request request,Boolean budgetariesScopeFunctionsIncludable) {
		validate(request,budgetariesScopeFunctionsIncludable);
		if(Boolean.TRUE.equals(request.getIsAdministrator())) {
			
		}else {
			if(request.getStatus() != null && RequestStatus.CODE_SUBMITTED.equals(request.getStatus().getCode()))
				throw new RuntimeException("Aucune modification possible car la demande a déja été soumise");
		}		
	}
	
	private static void validateProcess(Request request) {
		if(request.getStatus() != null && RequestStatus.CODE_ACCEPTED.equals(request.getStatus().getCode()))
			throw new RuntimeException("La demande a déja été acceptée");
		if(request.getStatus() != null && RequestStatus.CODE_REJECTED.equals(request.getStatus().getCode()))
			throw new RuntimeException("La demande a déja été rejetée");
	}
	
	private void setFields(Request request) {
		IdentificationFormQuerier.AbstractImpl.setFields(request.getType().getForm(), null);
		Map<String,IdentificationAttribute> attributs = Request.computeFieldsNames(request.getType().getForm());
		if(MapHelper.isNotEmpty(attributs)) {
			ThrowablesMessages throwablesMessages = new ThrowablesMessages();
			attributs.forEach( (fieldName,attribut) -> {
				Object fieldValue = FieldHelper.read(request, fieldName);
				if(Boolean.TRUE.equals(attribut.getRequired()))
					if(ValueHelper.isBlank(fieldValue))
						throwablesMessages.add(attribut.getName()+" est obligatoire");
			});
			throwablesMessages.throwIfNotEmpty();
		}
	}
	
	private void saveBudgetariesScopeFunctions(Request request,EntityManager entityManager) {
		org.cyk.utility.persistence.query.EntitySaver.Arguments<RequestScopeFunction> requestScopeFunctionsSaverArguments =
				new org.cyk.utility.persistence.query.EntitySaver.Arguments<RequestScopeFunction>().setEntityManager(entityManager);
		requestScopeFunctionsSaverArguments.setExistingCollection(RequestScopeFunctionQuerier.getInstance().readByRequestsIdentifiers(List.of(request.getIdentifier())));
		requestScopeFunctionsSaverArguments.setIsNotBelogingToProvidedCollectionDeletable(Boolean.TRUE);
		//build provided budgetaries scope functions
		if(CollectionHelper.isNotEmpty(request.getBudgetariesScopeFunctions())) {
			//get existing
			if(CollectionHelper.isNotEmpty(requestScopeFunctionsSaverArguments.getExistingCollection())) {				
				for(RequestScopeFunction requestScopeFunction : requestScopeFunctionsSaverArguments.getExistingCollection()) {
					if(request.getBudgetariesScopeFunctions().contains(requestScopeFunction.getScopeFunction())) {
						if(requestScopeFunctionsSaverArguments.getProvidedCollection() == null)
							requestScopeFunctionsSaverArguments.setProvidedCollection(new ArrayList<>());
						requestScopeFunctionsSaverArguments.getProvidedCollection().add(requestScopeFunction);
					}
				}
			}
			Collection<ScopeFunction> providedExistingScopeFunctions = CollectionHelper.isEmpty(requestScopeFunctionsSaverArguments.getProvidedCollection()) ? null
					: requestScopeFunctionsSaverArguments.getProvidedCollection().stream().map(x -> x.getScopeFunction()).collect(Collectors.toList());
			//get news
			for(ScopeFunction scopeFunction : request.getBudgetariesScopeFunctions()) {
				if(!CollectionHelper.contains(providedExistingScopeFunctions, scopeFunction)) {
					if(requestScopeFunctionsSaverArguments.getProvidedCollection() == null)
						requestScopeFunctionsSaverArguments.setProvidedCollection(new ArrayList<>());
					requestScopeFunctionsSaverArguments.getProvidedCollection().add(new RequestScopeFunction().setRequest(request).setScopeFunction(scopeFunction)
							.setRequested(Boolean.TRUE));
				}
			}
		}
		org.cyk.utility.persistence.query.EntitySaver.getInstance().save(RequestScopeFunction.class, requestScopeFunctionsSaverArguments);
	}
	
	@Override @Transactional
	public void initialize(Request request) {
		validate(request,Boolean.TRUE);
		request.set__auditFunctionality__("Initialisation");
		request.setStatus(EntityFinder.getInstance().find(RequestStatus.class, RequestStatus.CODE_INITIALIZED));
		setFields(request);
		request.setIdentifier(IdentifiableSystem.generateRandomly());
		request.setCode("D"+RandomHelper.getNumeric(3)+RandomHelper.getAlphabetic(4).toUpperCase()+RandomHelper.getNumeric(3));
		request.setCreationDate(LocalDateTime.now());
		if(request.getActor() == null) {
			request.setAccessToken(RandomHelper.getAlphanumeric(12));
		}
		EntityManager entityManager = __inject__(EntityManager.class);
		EntitySaver.getInstance().save(Request.class, new Arguments<Request>()
				.setPersistenceArguments(new org.cyk.utility.persistence.query.EntitySaver.Arguments<Request>().setEntityManager(entityManager)
						.setCreatables(List.of(request))));
		saveBudgetariesScopeFunctions(request,entityManager);

		notifyInitialized(request);	
	}
		
	@Override @Transactional
	public void record(Request request) {
		validateRecord(request,Boolean.TRUE);
		request.set__auditFunctionality__("Modification");
		setFields(request);
		EntityManager entityManager = __inject__(EntityManager.class);
		saveBudgetariesScopeFunctions(request,entityManager);
		EntitySaver.getInstance().save(Request.class, new Arguments<Request>()
				.setPersistenceArguments(new org.cyk.utility.persistence.query.EntitySaver.Arguments<Request>().setEntityManager(entityManager)
						.setUpdatables(List.of(request))));
	}
	
	@Override @Transactional
	public void recordPhoto(Request request) {
		validateRecord(request,Boolean.FALSE);
		request.set__auditFunctionality__("Modification Photo");
		EntitySaver.getInstance().save(Request.class, new Arguments<Request>()
				.setPersistenceArguments(new org.cyk.utility.persistence.query.EntitySaver.Arguments<Request>()
						.setUpdatables(List.of(request))));
	}
	
	@Override @Transactional
	public void recordPhotoByIdentifier(String identifier, byte[] bytes) {
		Request request = EntityFinder.getInstance().find(Request.class, new QueryExecutorArguments().addSystemIdentifiers(identifier)
				.setIsThrowExceptionIfIdentifierIsBlank(Boolean.TRUE)
				.setIsThrowExceptionIfResultIsBlank(Boolean.TRUE)
				).setPhoto(bytes);
		recordPhoto(request);
	}
	
	@Override @Transactional
	public void recordActOfAppointment(Request request) {
		validateRecord(request,Boolean.FALSE);
		request.set__auditFunctionality__("Modification Acte de nomination");
		EntitySaver.getInstance().save(Request.class, new Arguments<Request>()
				.setPersistenceArguments(new org.cyk.utility.persistence.query.EntitySaver.Arguments<Request>()
						.setUpdatables(List.of(request))));
	}
	
	@Override @Transactional
	public void recordActOfAppointmentByIdentifier(String identifier, byte[] bytes) {
		Request request = EntityFinder.getInstance().find(Request.class, new QueryExecutorArguments().addSystemIdentifiers(identifier)
				.setIsThrowExceptionIfIdentifierIsBlank(Boolean.TRUE)
				.setIsThrowExceptionIfResultIsBlank(Boolean.TRUE)
				).setActOfAppointment(bytes);
		recordActOfAppointment(request);
	}
	
	@Override @Transactional
	public void recordSignature(Request request) {
		validateRecord(request,Boolean.FALSE);
		EntitySaver.getInstance().save(Request.class, new Arguments<Request>()
				.setPersistenceArguments(new org.cyk.utility.persistence.query.EntitySaver.Arguments<Request>()
						.setUpdatables(List.of(request))));
	}
	
	@Override @Transactional
	public void recordSignatureByIdentifier(String identifier, byte[] bytes) {
		Request request = EntityFinder.getInstance().find(Request.class, new QueryExecutorArguments().addSystemIdentifiers(identifier)
				.setIsThrowExceptionIfIdentifierIsBlank(Boolean.TRUE)
				.setIsThrowExceptionIfResultIsBlank(Boolean.TRUE)
				).setSignature(bytes);
		recordSignature(request);
	}
	
	@Override @Transactional
	public void recordSignedRequestSheet(Request request) {
		validateRecord(request,Boolean.FALSE);
		EntitySaver.getInstance().save(Request.class, new Arguments<Request>()
				.setPersistenceArguments(new org.cyk.utility.persistence.query.EntitySaver.Arguments<Request>()
						.setUpdatables(List.of(request))));
	}
	
	@Override @Transactional
	public void recordSignedRequestSheetByIdentifier(String identifier,Boolean isAdministrator, byte[] bytes) {
		Request request = EntityFinder.getInstance().find(Request.class, new QueryExecutorArguments().addSystemIdentifiers(identifier)
				.setIsThrowExceptionIfIdentifierIsBlank(Boolean.TRUE)
				.setIsThrowExceptionIfResultIsBlank(Boolean.TRUE)
				).setIsAdministrator(isAdministrator).setSignedRequestSheet(bytes);
		recordSignedRequestSheet(request);
	}
	
	@Override @Transactional
	public void submit(Request request) {
		validate(request,Boolean.FALSE);
		if(request.getStatus() != null && RequestStatus.CODE_SUBMITTED.equals(request.getStatus().getCode()))
			throw new RuntimeException("La demande a déja été soumise");
		request.setStatus(EntityFinder.getInstance().find(RequestStatus.class, RequestStatus.CODE_SUBMITTED));
		request.set__auditFunctionality__("Soumission");
		EntitySaver.getInstance().save(Request.class, new Arguments<Request>()
				.setPersistenceArguments(new org.cyk.utility.persistence.query.EntitySaver.Arguments<Request>().setUpdatables(List.of(request))));
		
		//Non blocking operations
		try {
			notifySubmitted(request);
		} catch (Exception exception) {
			LogHelper.log(exception, getClass());
		}
	}
	
	@Override @Transactional
	public void submitByIdentifier(String identifier,String readPageURL) {
		Request request = EntityFinder.getInstance().find(Request.class, new QueryExecutorArguments().addSystemIdentifiers(identifier)
				.setIsThrowExceptionIfIdentifierIsBlank(Boolean.TRUE)
				.setIsThrowExceptionIfResultIsBlank(Boolean.TRUE)
				).setReadPageURL(readPageURL);
		submit(request);
	}
	
	@Override @Transactional
	public void return_(Request request) {
		validate(request,Boolean.FALSE);
		if(request.getStatus() != null && !RequestStatus.CODE_SUBMITTED.equals(request.getStatus().getCode()))
			throw new RuntimeException(String.format("La demande est %s. elle ne peut être retournée",request.getStatus().getName()));
		request.setStatus(EntityFinder.getInstance().find(RequestStatus.class, RequestStatus.CODE_INITIALIZED));
		request.set__auditFunctionality__("Retour");
		EntitySaver.getInstance().save(Request.class, new Arguments<Request>()
				.setPersistenceArguments(new org.cyk.utility.persistence.query.EntitySaver.Arguments<Request>().setUpdatables(List.of(request))));
		
		//Non blocking operations
		try {
			notifyReturned(request);
		} catch (Exception exception) {
			LogHelper.log(exception, getClass());
		}
	}
	
	@Override @Transactional
	public void returnByIdentifier(String identifier, String readPageURL) {
		Request request = EntityFinder.getInstance().find(Request.class, new QueryExecutorArguments().addSystemIdentifiers(identifier)
				.setIsThrowExceptionIfIdentifierIsBlank(Boolean.TRUE)
				.setIsThrowExceptionIfResultIsBlank(Boolean.TRUE)
				).setReadPageURL(readPageURL);
		return_(request);
	}
	
	@Override @Transactional
	public void accept(Request request) {
		validate(request,Boolean.FALSE);
		request.set__auditFunctionality__("Acceptation de demande");
		Collection<RequestScopeFunction> requestScopeFunctions = RequestScopeFunctionQuerier.getInstance().readByRequestsIdentifiers(List.of(request.getIdentifier()));
		accept(request, requestScopeFunctions
				, requestScopeFunctions.stream().map(x -> x.getScopeFunction()).collect(Collectors.toList())
				, request.getReadPageURL(), EntityManagerGetter.getInstance().get());
	}
	
	@Override @Transactional
	public void acceptByIdentifier(String identifier,Collection<String> grantedBudgetariesScopeFunctionsIdentifiers,String acceptationComment,String readPageURL,String auditActor) {
		Request request = EntityFinder.getInstance().find(Request.class, new QueryExecutorArguments().addSystemIdentifiers(identifier)
				.setIsThrowExceptionIfIdentifierIsBlank(Boolean.TRUE)
				.setIsThrowExceptionIfResultIsBlank(Boolean.TRUE)
				).setAcceptationComment(acceptationComment).setReadPageURL(readPageURL);
		request.set__auditWho__(auditActor);
		/*
		Collection<ScopeFunction> grantedScopeFunctions = null;
		//update granted
		Collection<RequestScopeFunction> requestScopeFunctions = RequestScopeFunctionQuerier.getInstance().readByRequestsIdentifiers(List.of(request.getIdentifier()));
		if(CollectionHelper.isNotEmpty(requestScopeFunctions)) {
			for(RequestScopeFunction requestScopeFunction : requestScopeFunctions) {
				requestScopeFunction
					.setGranted(CollectionHelper.contains(grantedBudgetariesScopeFunctionsIdentifiers, requestScopeFunction.getScopeFunction().getIdentifier()));
				if(Boolean.TRUE.equals(requestScopeFunction.getGranted())) {
					if(grantedScopeFunctions == null)
						grantedScopeFunctions = new ArrayList<>();
					grantedScopeFunctions.add(requestScopeFunction.getScopeFunction());
				}
			}
			EntityUpdater.getInstance().updateMany(CollectionHelper.cast(Object.class, requestScopeFunctions));
		}
		//create granted
		if(CollectionHelper.isNotEmpty(grantedBudgetariesScopeFunctionsIdentifiers)) {
			Collection<String> requestScopeFunctionsIdentifiers = CollectionHelper.isEmpty(requestScopeFunctions) ? null 
					: requestScopeFunctions.stream().map(x -> x.getScopeFunction().getIdentifier()).collect(Collectors.toList());
			Collection<Object> creatables = null;
			for(String grantedScopeFunctionIdentifier : grantedBudgetariesScopeFunctionsIdentifiers) {
				if(requestScopeFunctionsIdentifiers.contains(grantedScopeFunctionIdentifier))
					continue;
				if(creatables == null)
					creatables = new ArrayList<>();
				ScopeFunction scopeFunction = EntityFinder.getInstance().find(ScopeFunction.class, grantedScopeFunctionIdentifier);
				if(grantedScopeFunctions == null)
					grantedScopeFunctions = new ArrayList<>();
				grantedScopeFunctions.add(scopeFunction);
				creatables.add(new RequestScopeFunction().setRequest(request).setScopeFunction(scopeFunction).setRequested(Boolean.FALSE).setGranted(Boolean.TRUE));
			}
			if(CollectionHelper.isNotEmpty(creatables))
				EntityCreator.getInstance().createMany(creatables);
		}
		
		if(CollectionHelper.isEmpty(grantedScopeFunctions))
			throw new RuntimeException("Veuillez accorder au moins une fonction budgétaire");
		
		LogHelper.logInfo(String.format("%s fonction(s) budgétaire(s) accordée(s) : %s", grantedScopeFunctions.size(),grantedScopeFunctions.stream().map(x -> x.getCode())
				.collect(Collectors.joining(","))), getClass());
		
		accept(request,grantedScopeFunctions);
		*/
		accept(request, RequestScopeFunctionQuerier.getInstance().readByRequestsIdentifiers(List.of(request.getIdentifier()))
				, grantedBudgetariesScopeFunctionsIdentifiers.stream()
					.map(x -> EntityFinder.getInstance().find(ScopeFunction.class, x))
					.filter(x -> x != null)
					.collect(Collectors.toList())
				, readPageURL, EntityManagerGetter.getInstance().get());
	}
	
	public static void accept(Request request,Collection<RequestScopeFunction> requestScopeFunctions,Collection<ScopeFunction> grantedBudgetariesScopeFunctions
			,String readPageURL,EntityManager entityManager) {
		validate(request,Boolean.FALSE);
		validateProcess(request);		
		if(CollectionHelper.isEmpty(grantedBudgetariesScopeFunctions))
			throw new RuntimeException("Veuillez accorder au moins une fonction budgétaire");
		
		LogHelper.logInfo(String.format("%s fonction(s) budgétaire(s) accordée(s) : %s", grantedBudgetariesScopeFunctions.size(),grantedBudgetariesScopeFunctions.stream().map(x -> x.getCode())
				.collect(Collectors.joining(","))), RequestBusinessImpl.class);
		
		request.set__auditFunctionality__("Acceptation demande par bordereau");
		
		//update granted
		if(CollectionHelper.isNotEmpty(requestScopeFunctions)) {
			for(RequestScopeFunction requestScopeFunction : requestScopeFunctions) {
				if(Boolean.TRUE.equals(CollectionHelper.contains(grantedBudgetariesScopeFunctions, requestScopeFunction.getScopeFunction()))) {
					requestScopeFunction.setGranted(Boolean.TRUE);
					entityManager.merge(requestScopeFunction);
				}			
			}
		}
		//create granted
		if(CollectionHelper.isNotEmpty(grantedBudgetariesScopeFunctions)) {
			Collection<ScopeFunction> scopeFunctionss = CollectionHelper.isEmpty(requestScopeFunctions) ? null 
					: requestScopeFunctions.stream().map(x -> x.getScopeFunction()).collect(Collectors.toList());
			for(ScopeFunction grantedScopeFunction : grantedBudgetariesScopeFunctions) {
				if(scopeFunctionss.contains(grantedScopeFunction))
					continue;
				entityManager.persist(new RequestScopeFunction().setIdentifier(IdentifiableSystem.generateRandomly())
						.setRequest(request).setScopeFunction(grantedScopeFunction).setRequested(Boolean.FALSE).setGranted(Boolean.TRUE));
			}
		}

		request.setStatus(EntityFinder.getInstance().find(RequestStatus.class, RequestStatus.CODE_ACCEPTED));
		request.setProcessingDate(LocalDateTime.now());
		
		entityManager.merge(request);
			
		//Non blocking operations
		try {
			notifyAccepted(request,grantedBudgetariesScopeFunctions);
		} catch (Exception exception) {
			LogHelper.log(exception, RequestBusinessImpl.class);
		}
	}
	
	@Override @Transactional
	public void reject(Request request) {
		validate(request,Boolean.FALSE);
		validateProcess(request);		
		if(StringHelper.isBlank(request.getRejectionReason()))
			throw new RuntimeException("Le motif de rejet est obligatoire");
		request.set__auditFunctionality__("Rejet demande");
		request.setStatus(EntityFinder.getInstance().find(RequestStatus.class, RequestStatus.CODE_REJECTED));
		request.setProcessingDate(LocalDateTime.now());
		EntitySaver.getInstance().save(Request.class, new Arguments<Request>()
			.setPersistenceArguments(new org.cyk.utility.persistence.query.EntitySaver.Arguments<Request>().setUpdatables(List.of(request))));
	}
	
	@Override @Transactional
	public void rejectByIdentifier(String identifier, String rejectionReason,String readPageURL,String auditActor) {
		Request request = EntityFinder.getInstance().find(Request.class, new QueryExecutorArguments().addSystemIdentifiers(identifier)
				.setIsThrowExceptionIfIdentifierIsBlank(Boolean.TRUE)
				.setIsThrowExceptionIfResultIsBlank(Boolean.TRUE)
				).setReadPageURL(readPageURL);
		request.setRejectionReason(rejectionReason);
		request.set__auditWho__(auditActor);
		reject(request);
	}

	@Override
	public Integer notifyAccessTokens(String electronicMailAddress,String readPageURL) {
		if(StringHelper.isBlank(electronicMailAddress))
			throw new RuntimeException("L'adresse mail est obligatoire");
		Collection<Request> requests = RequestQuerier.getInstance().readByElectronicMailAddress(electronicMailAddress);
		if(CollectionHelper.isEmpty(requests))
			return null;
		requests.forEach(request -> {
			request.setReadPageURL(readPageURL);
			notifyAccessToken(request);
		});
		LogHelper.logInfo(String.format("%s notification(s) envoyée(s)", requests.size()), getClass());
		return requests.size();
	}
	
	/**/
	
	public static Boolean isNotifiableByEmail(Request request,Boolean loggableIfNot) {
		if(request.getType() != null && Boolean.FALSE.equals(request.getType().getNotifiableByEmail())) {
			if(Boolean.TRUE.equals(loggableIfNot))
				LogHelper.logWarning(String.format("Aucune notification à envoyer par mail car le type de demande %s ne le permet pas",request.getType().getCode()), RequestBusinessImpl.class);
			return Boolean.FALSE;
		}
		return Boolean.TRUE;
	}
	
	private static void setReadPageURL(Request request) {
		if(StringHelper.isBlank(request.getReadPageURL()))
			request.setReadPageURL(ValueHelper.defaultToIfBlank(ConfigurationHelper.getValueAsString(FreeMarker.VARIABLE_NAME_REQUEST_READ_PAGE_URL)
					, "http://siib"+("test".equals(ConfigurationHelper.getValueAsString("SIIB_ENVIRONMENT")) ? "test" : "")+".dgbf.ci/acteur/public/request/read.jsf"));
	}
	
	private static void notifyAccessToken(Request request) {
		ThrowableHelper.throwIllegalArgumentExceptionIfNull("request", request);
		setReadPageURL(request);
		new Thread(new Runnable() {
			@Override
			public void run() {
				try {
					String message = FreeMarker.getRequestAccessTokenMailMessage(request
							,RestHelper.buildResourceIdentifier(REPRESENTATION_PATH, REPRESENTATION_PATH_BUILD_REPORT_BY_IDENTIFIER));
					MailSender.getInstance().send("SIGOBE - "+request.getType().getName(), message, request.getElectronicMailAddress());
				} catch (Exception exception) {
					LogHelper.log(exception, getClass());
				}
			}
		}).start();
	}
	
	private static void notifyInitialized(Request request) {
		if(!Boolean.TRUE.equals(isNotifiableByEmail(request, Boolean.TRUE)))
			return;
		setReadPageURL(request);
		new Thread(new Runnable() {				
			@Override
			public void run() {
				try {
					String message = FreeMarker.getRequestInitializedMailMessage(request
							,RestHelper.buildResourceIdentifier(REPRESENTATION_PATH, REPRESENTATION_PATH_BUILD_REPORT_BY_IDENTIFIER));
					MailSender.getInstance().send("SIGOBE - "+request.getType().getName(), message, request.getElectronicMailAddress());
				} catch (Exception exception) {
					LogHelper.log(exception, getClass());
				}	
			}
		}).start();		
	}
	
	private static void notifySubmitted(Request request) {
		if(!Boolean.TRUE.equals(isNotifiableByEmail(request, Boolean.TRUE)))
			return;
		setReadPageURL(request);
		new Thread(new Runnable() {				
			@Override
			public void run() {
				try {
					org.cyk.utility.mail.Message message = new org.cyk.utility.mail.Message();
					message.setSubject("SIGOBE - "+request.getType().getName());
					message.setBody(FreeMarker.getRequestSubmittedMailMessage(request));
					message.addReceivers(List.of(request.getElectronicMailAddress()));
					ByteArrayOutputStream byteArrayOutputStream = ReportGetter.getInstance().get(request.getType().getReportIdentifier()
							,Map.of("identifiant",request.getIdentifier()),FileType.PDF);
					if(byteArrayOutputStream == null)
						LogHelper.logSevere(String.format("Le flux de la fiche d'identification de %s est null", request.getIdentifier()), getClass());
					else
						message.addAttachments(new org.cyk.utility.mail.Message.Attachment().setBytes(byteArrayOutputStream.toByteArray()).setExtension("pdf")
								.setName("fiche_d_identification"));
					MailSender.getInstance().send(message);
				} catch (Exception exception) {
					LogHelper.log(exception, getClass());
				}	
			}
		}).start();		
	}
	
	private static void notifyReturned(Request request) {
		if(!Boolean.TRUE.equals(isNotifiableByEmail(request, Boolean.TRUE)))
			return;
		setReadPageURL(request);
		new Thread(new Runnable() {				
			@Override
			public void run() {
				try {
					org.cyk.utility.mail.Message message = new org.cyk.utility.mail.Message();
					message.setSubject("SIGOBE - "+request.getType().getName());
					message.setBody(FreeMarker.getRequestReturnedMailMessage(request));
					message.addReceivers(List.of(request.getElectronicMailAddress()));
					MailSender.getInstance().send(message);
				} catch (Exception exception) {
					LogHelper.log(exception, getClass());
				}	
			}
		}).start();		
	}
	
	private static void notifyAccepted(Request request,Collection<ScopeFunction> scopeFunctions) {
		if(!Boolean.TRUE.equals(isNotifiableByEmail(request, Boolean.TRUE)))
			return;
		setReadPageURL(request);
		new Thread(new Runnable() {				
			@Override
			public void run() {
				try {
					org.cyk.utility.mail.Message message = new org.cyk.utility.mail.Message();
					message.setSubject("SIGOBE - "+request.getType().getName());
					message.setBody(FreeMarker.getRequestAcceptedMailMessage(request));
					message.addReceivers(List.of(request.getElectronicMailAddress()));
					if(CollectionHelper.isNotEmpty(scopeFunctions)) {
						for(ScopeFunction scopeFunction : scopeFunctions) {
							if(scopeFunction.getFunction().isCodeBelongsToExecutionAssisantsCodes())
								continue;
							String reportIdentifier = null;
							if(Function.CODE_CREDIT_MANAGER_HOLDER.equals(scopeFunction.getFunction().getCode()))
								reportIdentifier = request.getType().getCreditManagerSignatureSpecimenReportIdentifier();
							else if(Function.CODE_AUTHORIZING_OFFICER_HOLDER.equals(scopeFunction.getFunction().getCode()))
								reportIdentifier = request.getType().getAuthorizingOfficerSignatureSpecimenReportIdentifier();
							
							if(StringHelper.isBlank(reportIdentifier))
								continue;
							
							ByteArrayOutputStream byteArrayOutputStream = ReportGetter.getInstance().get(reportIdentifier
									,Map.of("identifiant",request.getIdentifier(),"poste",scopeFunction.getIdentifier()),FileType.PDF);
							if(byteArrayOutputStream == null) {
								LogHelper.logSevere(String.format("Le flux du spécimen de signature de %s est null", request.getIdentifier()), getClass());
								continue;
							}
							message.addAttachments(new org.cyk.utility.mail.Message.Attachment().setBytes(byteArrayOutputStream.toByteArray()).setExtension("pdf")
									.setName("specimen_de_signature_"+scopeFunction.getFunction().getCode()));
						}
						LogHelper.logInfo(String.format("%s fichier(s) joint(s) à envoyer", CollectionHelper.getSize(message.getAttachments())), getClass());
					}
					MailSender.getInstance().send(message);
				} catch (Exception exception) {
					LogHelper.log(exception, getClass());
				}
			}
		}).start();
	}
	
	@Override
	public void exportForAccountCreation(String actorCode) {
		actorCode = ValueHelper.defaultToIfBlank(actorCode, EntityLifeCycleListener.AbstractImpl.DEFAULT_USER_NAME);
		RequestQuerier.getInstance().exportForAccountCreation(actorCode, "export_pour_creation_compte", EntityLifeCycleListener.Event.READ.getValue(), new Date());
	}
}