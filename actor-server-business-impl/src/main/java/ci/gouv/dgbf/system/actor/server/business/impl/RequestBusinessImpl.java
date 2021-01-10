package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.enterprise.context.ApplicationScoped;
import javax.persistence.EntityManager;
import javax.transaction.Transactional;

import org.cyk.utility.__kernel__.business.EntityCreator;
import org.cyk.utility.__kernel__.business.EntitySaver;
import org.cyk.utility.__kernel__.business.EntitySaver.Arguments;
import org.cyk.utility.__kernel__.business.EntityUpdater;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.configuration.ConfigurationHelper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.log.LogHelper;
import org.cyk.utility.__kernel__.map.MapHelper;
import org.cyk.utility.__kernel__.object.marker.IdentifiableSystem;
import org.cyk.utility.__kernel__.persistence.query.EntityFinder;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.protocol.smtp.MailSender;
import org.cyk.utility.__kernel__.random.RandomHelper;
import org.cyk.utility.__kernel__.rest.RestHelper;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.throwable.ThrowableHelper;
import org.cyk.utility.__kernel__.throwable.ThrowablesMessages;
import org.cyk.utility.__kernel__.value.ValueHelper;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.RequestBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.RequestPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.IdentificationFormQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentificationAttribute;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestStatus;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.impl.FreeMarker;

@ApplicationScoped
public class RequestBusinessImpl extends AbstractBusinessEntityImpl<Request, RequestPersistence> implements RequestBusiness,Serializable {
	private static final long serialVersionUID = 1L;
	
	private void validate(Request request) {
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
	}
	
	private void validateProcess(Request request) {
		if(request.getStatus() != null 
			&& (RequestStatus.CODE_ACCEPTED.equals(request.getStatus().getCode()) || RequestStatus.CODE_REJECTED.equals(request.getStatus().getCode())) 
		)
			throw new RuntimeException("La demande a déja été traitée");
	}
	
	private void validateRecord(Request request) {
		validate(request);
		if(Boolean.TRUE.equals(request.getIsAdministrator())) {
			
		}else {
			if(request.getStatus() != null && RequestStatus.CODE_SUBMITTED.equals(request.getStatus().getCode()))
				throw new RuntimeException("Aucune modification possible car la demande a déja été soumise");
		}		
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
		org.cyk.utility.__kernel__.persistence.EntitySaver.Arguments<RequestScopeFunction> requestScopeFunctionsSaverArguments =
				new org.cyk.utility.__kernel__.persistence.EntitySaver.Arguments<RequestScopeFunction>().setEntityManager(entityManager);
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
		org.cyk.utility.__kernel__.persistence.EntitySaver.getInstance().save(RequestScopeFunction.class, requestScopeFunctionsSaverArguments);
	}
	
	@Override @Transactional
	public void initialize(Request request) {
		validate(request);
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
				.setPersistenceArguments(new org.cyk.utility.__kernel__.persistence.EntitySaver.Arguments<Request>().setEntityManager(entityManager)
						.setCreatables(List.of(request))));
		saveBudgetariesScopeFunctions(request,entityManager);

		notifyInitialized(request);	
	}
		
	@Override @Transactional
	public void record(Request request) {
		validateRecord(request);
		setFields(request);
		EntityManager entityManager = __inject__(EntityManager.class);
		saveBudgetariesScopeFunctions(request,entityManager);
		EntitySaver.getInstance().save(Request.class, new Arguments<Request>()
				.setPersistenceArguments(new org.cyk.utility.__kernel__.persistence.EntitySaver.Arguments<Request>().setEntityManager(entityManager)
						.setUpdatables(List.of(request))));
	}
	
	@Override @Transactional
	public void recordPhoto(Request request) {
		validateRecord(request);
		EntitySaver.getInstance().save(Request.class, new Arguments<Request>()
				.setPersistenceArguments(new org.cyk.utility.__kernel__.persistence.EntitySaver.Arguments<Request>()
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
		validateRecord(request);
		EntitySaver.getInstance().save(Request.class, new Arguments<Request>()
				.setPersistenceArguments(new org.cyk.utility.__kernel__.persistence.EntitySaver.Arguments<Request>()
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
		validateRecord(request);
		EntitySaver.getInstance().save(Request.class, new Arguments<Request>()
				.setPersistenceArguments(new org.cyk.utility.__kernel__.persistence.EntitySaver.Arguments<Request>()
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
		validateRecord(request);
		EntitySaver.getInstance().save(Request.class, new Arguments<Request>()
				.setPersistenceArguments(new org.cyk.utility.__kernel__.persistence.EntitySaver.Arguments<Request>()
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
		validate(request);
		if(request.getStatus() != null && RequestStatus.CODE_SUBMITTED.equals(request.getStatus().getCode()))
			throw new RuntimeException("La demande a déja été soumise");
		request.setStatus(EntityFinder.getInstance().find(RequestStatus.class, RequestStatus.CODE_SUBMITTED));
		EntitySaver.getInstance().save(Request.class, new Arguments<Request>()
				.setPersistenceArguments(new org.cyk.utility.__kernel__.persistence.EntitySaver.Arguments<Request>().setUpdatables(List.of(request))));
	}
	
	@Override @Transactional
	public void submitByIdentifier(String identifier) {
		Request request = EntityFinder.getInstance().find(Request.class, new QueryExecutorArguments().addSystemIdentifiers(identifier)
				.setIsThrowExceptionIfIdentifierIsBlank(Boolean.TRUE)
				.setIsThrowExceptionIfResultIsBlank(Boolean.TRUE)
				);
		submit(request);
	}
	
	@Override @Transactional
	public void accept(Request request) {
		validate(request);
		validateProcess(request);
		/*
		if(RequestType.CODE_DEMANDE_POSTES_BUDGETAIRES.equals(request.getType().getCode())) {
			if(request.getSignedRequestSheet() == null)//TODO this has to be done based on required
				throw new RuntimeException("Le spécimen de signature est obligatoire");
		}
		*/
		request.setStatus(EntityFinder.getInstance().find(RequestStatus.class, RequestStatus.CODE_ACCEPTED));
		request.setProcessingDate(LocalDateTime.now());
		EntitySaver.getInstance().save(Request.class, new Arguments<Request>()
				.setPersistenceArguments(new org.cyk.utility.__kernel__.persistence.EntitySaver.Arguments<Request>().setUpdatables(List.of(request))));
		//Non blocking operations
		try {
			notifyAccepted(request);
		} catch (Exception exception) {
			LogHelper.log(exception, getClass());
		}
	}
	
	@Override @Transactional
	public void acceptByIdentifier(String identifier,Collection<String> budgetariesScopeFunctionsIdentifiers,String comment,byte[] signedRequestSheetBytes) {
		Request request = EntityFinder.getInstance().find(Request.class, new QueryExecutorArguments().addSystemIdentifiers(identifier)
				.setIsThrowExceptionIfIdentifierIsBlank(Boolean.TRUE)
				.setIsThrowExceptionIfResultIsBlank(Boolean.TRUE)
				);
		//update granted
		Collection<RequestScopeFunction> requestScopeFunctions = RequestScopeFunctionQuerier.getInstance().readByRequestsIdentifiers(List.of(request.getIdentifier()));
		if(CollectionHelper.isNotEmpty(requestScopeFunctions)) {
			for(RequestScopeFunction requestScopeFunction : requestScopeFunctions) {
				requestScopeFunction
					.setGranted(CollectionHelper.contains(budgetariesScopeFunctionsIdentifiers, requestScopeFunction.getScopeFunction().getIdentifier()));
			}
			EntityUpdater.getInstance().updateMany(CollectionHelper.cast(Object.class, requestScopeFunctions));
		}
		//create granted
		if(CollectionHelper.isNotEmpty(budgetariesScopeFunctionsIdentifiers)) {
			Collection<String> requestScopeFunctionsIdentifiers = CollectionHelper.isEmpty(requestScopeFunctions) ? null 
					: requestScopeFunctions.stream().map(x -> x.getScopeFunction().getIdentifier()).collect(Collectors.toList());
			Collection<Object> creatables = null;
			for(String scopeFunctionIdentifier : budgetariesScopeFunctionsIdentifiers) {
				if(requestScopeFunctionsIdentifiers.contains(scopeFunctionIdentifier))
					continue;
				if(creatables == null)
					creatables = new ArrayList<>();
				creatables.add(new RequestScopeFunction().setRequest(request).setScopeFunction(EntityFinder.getInstance().find(ScopeFunction.class
					, scopeFunctionIdentifier)).setRequested(Boolean.FALSE).setGranted(Boolean.TRUE));
			}
			if(CollectionHelper.isNotEmpty(creatables))
				EntityCreator.getInstance().createMany(creatables);
		}
		request.setComment(comment);
		//request.setSignedRequestSheet(signedRequestSheetBytes);
		accept(request);
	}
	
	@Override @Transactional
	public void reject(Request request) {
		validate(request);
		validateProcess(request);
		if(StringHelper.isBlank(request.getRejectionReason()))
			throw new RuntimeException("Le motif de rejet est obligatoire");
		request.setStatus(EntityFinder.getInstance().find(RequestStatus.class, RequestStatus.CODE_REJECTED));
		request.setProcessingDate(LocalDateTime.now());
		EntitySaver.getInstance().save(Request.class, new Arguments<Request>()
				.setPersistenceArguments(new org.cyk.utility.__kernel__.persistence.EntitySaver.Arguments<Request>().setUpdatables(List.of(request))));
	}
	
	@Override @Transactional
	public void rejectByIdentifier(String identifier, String rejectionReason) {
		Request request = EntityFinder.getInstance().find(Request.class, new QueryExecutorArguments().addSystemIdentifiers(identifier)
				.setIsThrowExceptionIfIdentifierIsBlank(Boolean.TRUE)
				.setIsThrowExceptionIfResultIsBlank(Boolean.TRUE)
				);
		request.setRejectionReason(rejectionReason);
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
	
	private static void notifyAccessToken(Request request) {
		ThrowableHelper.throwIllegalArgumentExceptionIfNull("request", request);
		if(StringHelper.isBlank(request.getReadPageURL()))
			request.setReadPageURL(ValueHelper.defaultToIfBlank(ConfigurationHelper.getValueAsString(FreeMarker.VARIABLE_NAME_REQUEST_READ_PAGE_URL)
					, "http://siib"+("test".equals(ConfigurationHelper.getValueAsString("SIIB_ENVIRONMENT")) ? "test" : "")+".dgbf.ci/acteur/public/request/read.jsf"));
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
	
	private static void notifyAccepted(Request request) {
		new Thread(new Runnable() {				
			@Override
			public void run() {
				try {
					MailSender.getInstance().send("SIGOBE - "+request.getType().getName(), FreeMarker.getRequestAcceptedMailMessage(request), request.getElectronicMailAddress());
				} catch (Exception exception) {
					LogHelper.log(exception, getClass());
				}	
			}
		}).start();		
	}
}