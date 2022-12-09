package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Collection;
import java.util.stream.Collectors;

import javax.enterprise.context.ApplicationScoped;
import javax.persistence.EntityManager;
import javax.transaction.Transactional;

import org.cyk.utility.__kernel__.array.ArrayHelper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.number.NumberHelper;
import org.cyk.utility.persistence.EntityManagerGetter;
import org.cyk.utility.persistence.query.EntityCreator;
import org.cyk.utility.persistence.query.EntityFinder;
import org.cyk.utility.__kernel__.random.RandomHelper;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.throwable.ThrowablesMessages;
import org.cyk.utility.__kernel__.time.TimeHelper;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.RequestBusiness;
import ci.gouv.dgbf.system.actor.server.business.api.RequestDispatchSlipBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.RequestDispatchSlipPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestDispatchSlip;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestStatus;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

@ApplicationScoped
public class RequestDispatchSlipBusinessImpl extends AbstractBusinessEntityImpl<RequestDispatchSlip, RequestDispatchSlipPersistence> implements RequestDispatchSlipBusiness,Serializable {
	private static final long serialVersionUID = 1L;

	private static void validate(RequestDispatchSlip requestDispatchSlip,Boolean requestsValidatable) {
		if(requestDispatchSlip == null)
			throw new RuntimeException("Le bordereau est obligatoire");
		if(StringHelper.isBlank(requestDispatchSlip.getBudgetCategoryIdentifier()))
			throw new RuntimeException("La catégorie de budget du bordereau est obligatoire");
		if(requestDispatchSlip.getSection() == null)
			throw new RuntimeException("La section du bordereau est obligatoire");
		if(requestDispatchSlip.getFunction() == null)
			throw new RuntimeException("La catégorie de fonction budgétaire du bordereau est obligatoire");
		if(requestsValidatable == null || Boolean.TRUE.equals(requestsValidatable)) {
			if(CollectionHelper.isEmpty(requestDispatchSlip.getRequests()))
				throw new RuntimeException("Le bordereau doit contenir au moins une demande");
			Collection<Request> requests = requestDispatchSlip.getRequests().stream().filter(x -> x.getDispatchSlip() != null && !x.getDispatchSlip().equals(requestDispatchSlip)).collect(Collectors.toList());
			if(CollectionHelper.isNotEmpty(requests))
				throw new RuntimeException(String.format("Il existe %s demande(s) déja sur bordereau.",requests.size()));
			if(StringHelper.isNotBlank(requestDispatchSlip.getIdentifier())) {
				Collection<RequestScopeFunction> requestScopeFunctions = RequestScopeFunctionQuerier.getInstance()
					.readByRequestsIdentifiers(requestDispatchSlip.getRequests().stream()
					.map(request -> request.getIdentifier()).collect(Collectors.toList()));
				ThrowablesMessages throwablesMessages = new ThrowablesMessages();
				for(Request request : requestDispatchSlip.getRequests()) {
					if(request.getAdministrativeUnit().getSection() == null)
						throwablesMessages.add(String.format("La demande N° %s doit avoir une section afin d'être portée sur un bordereau"
								,request.getCode()));
					if(!requestDispatchSlip.getSection().equals(request.getAdministrativeUnit().getSection()))
						throwablesMessages.add(String.format("La demande N° %s ayant pour section %s ne peut être sur un bordereau de la section %s"
								,request.getCode(),request.getAdministrativeUnit().getSection().getCode(),requestDispatchSlip.getSection().getCode()));
					
					Collection<ScopeFunction> scopeFunctions = requestScopeFunctions.stream()
							.filter(requestScopeFunction -> requestScopeFunction.getRequest().equals(request) 
									&& requestScopeFunction.getScopeFunction().getFunction().equals(requestDispatchSlip.getFunction()))
							.collect(Collectors.toList()).stream().map(requestScopeFunction -> requestScopeFunction.getScopeFunction()).collect(Collectors.toList());
					if(CollectionHelper.isEmpty(scopeFunctions))
						throwablesMessages.add(String.format("La demande N° %s doit avoir une fonction budgétaire de catégorie %s"
								,request.getCode(),requestDispatchSlip.getFunction().getName()));
				}
				throwablesMessages.throwIfNotEmpty();
			}
		}		
	}
	
	private static String generateCode(RequestDispatchSlip requestDispatchSlip) {
		StringBuilder stringBuilder = new StringBuilder("B");
		stringBuilder.append(requestDispatchSlip.getFunction().getCode());
		stringBuilder.append(requestDispatchSlip.getSection().getCode());		
		stringBuilder.append(TimeHelper.formatLocalDateTime(requestDispatchSlip.getCreationDate(), "yyMMdd"));
		stringBuilder.append(RandomHelper.getAlphabetic(2).toUpperCase());
		return stringBuilder.toString();
	}
	
	public static void record(RequestDispatchSlip requestDispatchSlip,EntityManager entityManager) {
		validate(requestDispatchSlip,Boolean.TRUE);
		if(requestDispatchSlip.getProcessingDate() != null)
			throw new RuntimeException("Impossible de modifier le bordereau car il a déja été traité");
		if(requestDispatchSlip.getSendingDate() != null)
			throw new RuntimeException("Impossible de modifier le bordereau car il a déja été transmis");
		if(requestDispatchSlip.getIdentifier() == null) {
			requestDispatchSlip.set__auditFunctionality__("Création bordereau");
			//if(StringHelper.isBlank(requestDispatchSlip.getCode()))
			//	requestDispatchSlip.setCode(String.format("%s%s%s","B","2021",RandomHelper.getAlphanumeric(5).toUpperCase()));
			if(StringHelper.isBlank(requestDispatchSlip.getName()))
				requestDispatchSlip.setName(String.format("Bordereau de demandes de %s de la section %s",requestDispatchSlip.getFunction().getName()
						,requestDispatchSlip.getSection().getCode()));
			requestDispatchSlip.setCreationDate(LocalDateTime.now());
			if(StringHelper.isBlank(requestDispatchSlip.getCode()))
				requestDispatchSlip.setCode(generateCode(requestDispatchSlip));
			EntityCreator.getInstance().createOne(requestDispatchSlip,entityManager);
			//entityManager.persist(requestDispatchSlip);
			updateDispatchSlip(requestDispatchSlip, entityManager);
		}else {
			requestDispatchSlip.set__auditFunctionality__("Modification bordereau");
			Collection<Request> requests = RequestQuerier.getInstance().readByDispatchSlipIdentifier(requestDispatchSlip.getIdentifier());
			if(CollectionHelper.isEmpty(requests)) {
				updateDispatchSlip(requestDispatchSlip, entityManager);
			}else {
				for(Request request : requests)
					if(!requestDispatchSlip.getRequests().contains(request)) {
						request.setDispatchSlip(null);
						entityManager.merge(request);
					}
				updateDispatchSlip(requestDispatchSlip, entityManager);
			}
			entityManager.merge(requestDispatchSlip);
		}		
	}
	
	@Override @Transactional
	public void record(RequestDispatchSlip requestDispatchSlip) {
		record(requestDispatchSlip, EntityManagerGetter.getInstance().get());
		/*
		validate(requestDispatchSlip,Boolean.TRUE);
		if(requestDispatchSlip.getProcessingDate() != null)
			throw new RuntimeException("Impossible de modifier le bordereau car il a déja été traité");
		if(requestDispatchSlip.getSendingDate() != null)
			throw new RuntimeException("Impossible de modifier le bordereau car il a déja été transmis");
		EntityManager entityManager = EntityManagerGetter.getInstance().get();
		if(requestDispatchSlip.getIdentifier() == null) {
			requestDispatchSlip.set__auditFunctionality__("Création bordereau");
			//if(StringHelper.isBlank(requestDispatchSlip.getCode()))
			//	requestDispatchSlip.setCode(String.format("%s%s%s","B","2021",RandomHelper.getAlphanumeric(5).toUpperCase()));
			if(StringHelper.isBlank(requestDispatchSlip.getName()))
				requestDispatchSlip.setName(String.format("Bordereau de demandes de %s de la section %s",requestDispatchSlip.getFunction().getName()
						,requestDispatchSlip.getSection().getCode()));
			requestDispatchSlip.setCreationDate(LocalDateTime.now());
			if(StringHelper.isBlank(requestDispatchSlip.getCode()))
				requestDispatchSlip.setCode(generateCode(requestDispatchSlip));
			EntityCreator.getInstance().createOne(requestDispatchSlip,entityManager);
			//entityManager.persist(requestDispatchSlip);
			updateDispatchSlip(requestDispatchSlip, entityManager);
		}else {
			requestDispatchSlip.set__auditFunctionality__("Modification bordereau");
			Collection<Request> requests = RequestQuerier.getInstance().readByDispatchSlipIdentifier(requestDispatchSlip.getIdentifier());
			if(CollectionHelper.isEmpty(requests)) {
				updateDispatchSlip(requestDispatchSlip, entityManager);
			}else {
				for(Request request : requests)
					if(!requestDispatchSlip.getRequests().contains(request)) {
						request.setDispatchSlip(null);
						entityManager.merge(request);
					}
				updateDispatchSlip(requestDispatchSlip, entityManager);
			}
			entityManager.merge(requestDispatchSlip);
		}
		*/
	}
	
	private static void updateDispatchSlip(RequestDispatchSlip requestDispatchSlip,EntityManager entityManager) {
		for(Request request : requestDispatchSlip.getRequests()) {
			if(request.getDispatchSlip() != null && request.getDispatchSlip().equals(requestDispatchSlip))
				continue;
			request.set__auditFunctionality__(requestDispatchSlip.get__auditFunctionality__());
			request.setDispatchSlip(requestDispatchSlip);
			entityManager.merge(request);
		}
	}
	
	@Override @Transactional
	public void send(RequestDispatchSlip requestDispatchSlip) {
		if(requestDispatchSlip != null) {
			requestDispatchSlip.setRequests(RequestQuerier.getInstance().readByDispatchSlipIdentifier(requestDispatchSlip.getIdentifier()));
		}
		validate(requestDispatchSlip,Boolean.TRUE);
		if(requestDispatchSlip.getProcessingDate() != null)
			throw new RuntimeException("Impossible de transmettre le bordereau car il a déja été traité");
		if(requestDispatchSlip.getSendingDate() != null)
			throw new RuntimeException("Le bordereau a déja été transmis");
		EntityManager entityManager = EntityManagerGetter.getInstance().get();
		requestDispatchSlip.set__auditFunctionality__("Transmission bordereau");
		for(Request request : requestDispatchSlip.getRequests()) {
			request.set__auditFunctionality__(requestDispatchSlip.get__auditFunctionality__());
			entityManager.merge(request);
		}
		requestDispatchSlip.setSendingDate(LocalDateTime.now());
		entityManager.merge(requestDispatchSlip);
	}
	
	public static void process(RequestDispatchSlip requestDispatchSlip,EntityManager entityManager) {
		validate(requestDispatchSlip,Boolean.TRUE);
		if(requestDispatchSlip.getProcessingDate() != null)
			throw new RuntimeException("Le bordereau a déja été traité");
		requestDispatchSlip.set__auditFunctionality__("Traitement bordereau");
		requestDispatchSlip.setProcessingDate(LocalDateTime.now());
		entityManager.merge(requestDispatchSlip);
		
		Collection<RequestScopeFunction> grantedRequestScopeFunctions = RequestScopeFunctionQuerier.getInstance()
				.readByRequestsIdentifiers( requestDispatchSlip.getRequests().stream()
						.filter(request -> RequestStatus.CODE_ACCEPTED.equals(request.getStatusAsString()))
						.map(request -> request.getIdentifier())
						.collect(Collectors.toList()));
		
		RequestBusiness requestBusiness = __inject__(RequestBusiness.class);
		for(Request request : requestDispatchSlip.getRequests()) {
			if(RequestStatus.CODE_ACCEPTED.equals(request.getStatus().getCode()) || RequestStatus.CODE_REJECTED.equals(request.getStatus().getCode()))
				continue;
			request.set__auditWho__(requestDispatchSlip.get__auditWho__());
			if(RequestStatus.CODE_ACCEPTED.equals(request.getStatusAsString())) {
				RequestBusinessImpl.accept(request, grantedRequestScopeFunctions, grantedRequestScopeFunctions.stream()
						.filter(x -> x.getRequest().equals(request))
						.map(x -> x.getScopeFunction())
						.collect(Collectors.toList())
						, requestDispatchSlip.getReadPageURL(), entityManager);
			}else {
				requestBusiness.reject(request);
			}
		}
	}
	
	@Override @Transactional
	public void process(RequestDispatchSlip requestDispatchSlip) {
		process(requestDispatchSlip, EntityManagerGetter.getInstance().get());
	}
	
	public static void processIfAllRequestsProcessed(Collection<String> identifiers,EntityManager entityManager) {
		if(CollectionHelper.isEmpty(identifiers))
			return;
		identifiers.forEach(identifier -> {
			processIfAllRequestsProcessed(identifier, entityManager);
		});
	}
	
	public static void processIfAllRequestsProcessed(String identifier,EntityManager entityManager) {
		if(StringHelper.isBlank(identifier))
			return;
		Long numberOfRequestsNotProcessed = entityManager.createQuery("SELECT COUNT(r) FROM Request r WHERE r.dispatchSlip.identifier = :identifier AND r.status.code NOT IN :codes",Long.class)
			.setParameter("identifier", identifier).setParameter("codes", RequestStatus.CODES_ACCEPTED_REJECTED).getSingleResult();
		if(NumberHelper.isGreaterThanZero(numberOfRequestsNotProcessed))
			return;
		RequestDispatchSlip requestDispatchSlip = EntityFinder.getInstance().find(RequestDispatchSlip.class, identifier);
		if(requestDispatchSlip == null)
			return;
		LocalDateTime processingDate = entityManager.createQuery("SELECT MAX(r.processingDate) FROM Request r WHERE r.dispatchSlip.identifier = :identifier",LocalDateTime.class)
				.setParameter("identifier", identifier).getSingleResult();
		requestDispatchSlip.setProcessingDate(processingDate);
		entityManager.merge(requestDispatchSlip);
	}
	
	public static void processIfAllRequestsProcessed(EntityManager entityManager,String...identifiers) {
		if(ArrayHelper.isEmpty(identifiers))
			return;
		processIfAllRequestsProcessed(CollectionHelper.listOf(identifiers), entityManager);
	}
	
	public static void processIfAllRequestsProcessed(RequestDispatchSlip requestDispatchSlip,EntityManager entityManager) {
		if(requestDispatchSlip == null)
			return;
		processIfAllRequestsProcessed(entityManager, requestDispatchSlip.getIdentifier());
	}
}