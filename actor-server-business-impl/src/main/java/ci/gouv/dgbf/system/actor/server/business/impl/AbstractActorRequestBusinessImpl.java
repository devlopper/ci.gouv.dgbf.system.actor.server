package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.persistence.EntityManager;
import javax.transaction.Transactional;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.klass.ClassHelper;
import org.cyk.utility.__kernel__.map.MapHelper;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.throwable.ThrowableHelper;
import org.cyk.utility.__kernel__.throwable.ThrowablesMessages;
import org.cyk.utility.business.TransactionResult;
import org.cyk.utility.business.Validator;
import org.cyk.utility.business.server.AbstractSpecificBusinessImpl;
import org.cyk.utility.business.server.EntityCreator;
import org.cyk.utility.business.server.EntityDeletor;
import org.cyk.utility.business.server.EntityUpdater;
import org.cyk.utility.persistence.EntityManagerGetter;
import org.cyk.utility.persistence.query.EntityFinder;
import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.persistence.query.QueryName;

import ci.gouv.dgbf.system.actor.server.business.api.AbstractActorRequestBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.AbstractActorRequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AbstractActorRequest;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;

public abstract class AbstractActorRequestBusinessImpl<REQUEST extends AbstractActorRequest> extends AbstractSpecificBusinessImpl<REQUEST> implements AbstractActorRequestBusiness<REQUEST>,Serializable {
	private static final long serialVersionUID = 1L;

	protected abstract Class<?> getRequestableClass();
	protected abstract String getProcessActionIdentitifer();
	
	protected static <REQUESTABLE,REQUEST> TransactionResult record(Class<?> callerClass,Class<REQUEST> requestClass,Collection<String> actorsIdentifiers,Class<REQUESTABLE> requestableClass, Collection<String> requestablesIdentifiers,String actorCode,Boolean ignoreExisting,EntityManager entityManager) {
		ThrowableHelper.throwIllegalArgumentExceptionIfNull("caller class", callerClass);
		ThrowableHelper.throwIllegalArgumentExceptionIfNull("request class", requestClass);
		ThrowableHelper.throwIllegalArgumentExceptionIfNull("requestable class", requestableClass);
		if(CollectionHelper.isEmpty(actorsIdentifiers))
			throw new RuntimeException("Identifiants acteurs requis");
		if(CollectionHelper.isEmpty(requestablesIdentifiers))
			throw new RuntimeException("Identifiants objets demandés requis");
		if(StringHelper.isBlank(actorCode))
			throw new RuntimeException("Nom utilisateur requis");
		String requestableVariableName = StringHelper.getVariableNameFrom(requestableClass.getSimpleName());
		String query = String.format("SELECT t.actor.identifier,t.%1$s.identifier,t.actor.code,t.%1$s.code FROM %2$s t WHERE t.actor.identifier IN :actorsIdentifiers"				
				+ " AND t.%1$s.identifier IN :requestablesIdentifiers",requestableVariableName,requestClass.getSimpleName());
		@SuppressWarnings("unchecked")
		List<Object[]> arrays = entityManager.createQuery(query)
			.setParameter("actorsIdentifiers", actorsIdentifiers).setParameter("requestablesIdentifiers", requestablesIdentifiers).getResultList();
		if(CollectionHelper.isNotEmpty(arrays))
			throw new RuntimeException(String.format("Il existe %s demande(s) en cours de validation. <<%s>>",arrays.size()
					,arrays.stream().map(a -> a[2]+","+a[3]).collect(Collectors.joining("|"))));
		Collection<Actor> actors = EntityFinder.getInstance().findMany(Actor.class, actorsIdentifiers);
		if(CollectionHelper.isEmpty(actors))
			throw new RuntimeException("acteurs requis");
		Collection<REQUESTABLE> requestables = EntityFinder.getInstance().findMany(requestableClass, requestablesIdentifiers);
		if(CollectionHelper.isEmpty(requestables))
			throw new RuntimeException("objets demandés requis");
		Collection<Object> requests = new ArrayList<Object>();
		actors.forEach(actor -> {
			requestables.forEach(requestable -> {
				REQUEST request = ClassHelper.instanciate(requestClass);
				((AbstractActorRequest)request).setActor(actor).set__auditFunctionality__("Demande de domaine").set__auditWho__(actorCode);
				FieldHelper.write(request, requestableVariableName, requestable);
				requests.add(request);
			});
		});
		TransactionResult transactionResult = new TransactionResult().setName(String.format("Demande de domaines %s",requests)).setTupleName("Demande domaine");
		EntityCreator.getInstance().create(new QueryExecutorArguments().setObjects(requests).setEntityManager(entityManager));
		transactionResult.incrementNumberOfCreation(Long.valueOf(requests.size()));
		transactionResult.log(callerClass);
		return transactionResult;
	}
	
	@Override @Transactional
	public TransactionResult record(Collection<String> actorsIdentifiers, Collection<String> requestablesIdentifiers,String actorCode,Boolean ignoreExisting) {
		return record(getClass(),getEntityClass(),actorsIdentifiers,getRequestableClass(), requestablesIdentifiers, actorCode,ignoreExisting, EntityManagerGetter.getInstance().get());
	}
	
	/**/
	
	public static <REQUEST extends AbstractActorRequest> TransactionResult cancel(Class<?> callerClass,Class<REQUEST> requestClass,Collection<String> identifiers, String actorCode,Boolean ignoreExisting,EntityManager entityManager) {
		if(CollectionHelper.isEmpty(identifiers))
			throw new RuntimeException("Identifiants requis");
		if(StringHelper.isBlank(actorCode))
			throw new RuntimeException("Nom utilisateur requis");
		Collection<REQUEST> requests = EntityManagerGetter.getInstance().get()
				.createQuery(String.format("SELECT t FROM %s t WHERE t.identifier IN :identifiers AND t.granted IS NULL",requestClass.getSimpleName()), requestClass)
				.setParameter("identifiers", identifiers).getResultList();
		if(CollectionHelper.isEmpty(requests))
			throw new RuntimeException("domaines à annuler requis");
		requests.forEach(actorScopeRequest -> {
			actorScopeRequest.set__auditFunctionality__("Annulation de demande de domaine").set__auditWho__(actorCode);
		});
		TransactionResult transactionResult = new TransactionResult().setName(String.format("Annulation de demande de domaines %s",identifiers)).setTupleName("Demande domaine annulée");
		EntityDeletor.getInstance().delete(new QueryExecutorArguments().setObjects(CollectionHelper.cast(Object.class, requests)).setEntityManager(entityManager));
		transactionResult.incrementNumberOfDeletion(Long.valueOf(requests.size()));
		transactionResult.log(callerClass);
		return transactionResult;
	}
	
	@Override @Transactional
	public TransactionResult cancel(Collection<String> identifiers, String actorCode,Boolean ignoreExisting) {
		return cancel(getClass(),getEntityClass(),identifiers, actorCode,ignoreExisting, EntityManagerGetter.getInstance().get());
	}
	
	/**/
	
	public static interface Processing<REQUEST extends AbstractActorRequest>{
		void listenGrantIsTrue(REQUEST request, String actorCode,EntityManager entityManager);
		void listenGrantIsNotTrue(REQUEST request, String actorCode,EntityManager entityManager);
	}
	
	protected abstract Processing<REQUEST> getPocessing();
	
	public static <REQUEST extends AbstractActorRequest> TransactionResult process(Class<?> callerClass,Collection<REQUEST> requests, String actorCode,String processActionIdentifier,Processing<REQUEST> processing,EntityManager entityManager) {
		@SuppressWarnings("unchecked")
		Class<REQUEST> requestClass = (Class<REQUEST>) requests.iterator().next().getClass();
		ThrowablesMessages.throwIfNotEmpty(Validator.getInstance().validate(requestClass, requests, processActionIdentifier));
		TransactionResult transactionResult = new TransactionResult().setName(String.format("Traitement de demande de domaines")).setTupleName("Demande domaine traitée");
		EntityUpdater.getInstance().update(new QueryExecutorArguments().setObjects(CollectionHelper.cast(Object.class, requests)).setEntityManager(entityManager));
		transactionResult.incrementNumberOfUpdate(Long.valueOf(requests.size()));
		transactionResult.log(callerClass);
		requests.forEach(request -> {
			if(Boolean.TRUE.equals(request.getGranted()))
				processing.listenGrantIsTrue(request,actorCode,entityManager);
			else
				processing.listenGrantIsNotTrue(request,actorCode,entityManager);
		});
		return transactionResult;
	}
	
	public static <REQUEST extends AbstractActorRequest> TransactionResult process(Class<?> callerClass,Class<REQUEST> requestClass,Collection<String> identifiers,Map<String,Boolean> grants,Map<String,String> comments, String actorCode,String processActionIdentifier,Processing<REQUEST> processing,EntityManager entityManager) {
		if(CollectionHelper.isEmpty(identifiers))
			throw new RuntimeException("Identifiants requis");
		if(StringHelper.isBlank(actorCode))
			throw new RuntimeException("Nom utilisateur requis");
		Collection<REQUEST> requests = EntityFinder.getInstance().findMany(requestClass, identifiers);
		if(CollectionHelper.isEmpty(requests))
			throw new RuntimeException("domaines à annuler requis");
		requests.forEach(request -> {
			request.setGranted(MapHelper.readByKey(grants, request.getIdentifier()));
			request.setComment(MapHelper.readByKey(comments, request.getIdentifier()));
		});
		return process(callerClass,requests, actorCode,processActionIdentifier,processing, entityManager);
	}
	
	@Override @Transactional
	public TransactionResult process(Collection<String> identifiers,Map<String,Boolean> grants,Map<String,String> comments, String actorCode) {
		return process(getClass(),getEntityClass(),identifiers,grants,comments,actorCode,getProcessActionIdentitifer(),getPocessing(), EntityManagerGetter.getInstance().get());
	}
	
	@Override @Transactional
	public TransactionResult process(Collection<REQUEST> requests, String actorCode) {
		return process(getClass(),requests,getProcessActionIdentitifer(), actorCode,getPocessing(), EntityManagerGetter.getInstance().get());
	}

	/**/
	
	@Override
	public Collection<REQUEST> findByActorCode(String actorCode, Boolean processed, Boolean granted, Boolean pageable,Integer firstTupleIndex, Integer numberOfTuples) {
		ThrowablesMessages throwablesMessages = new ThrowablesMessages();
		ValidatorImpl.validateActorCode(actorCode, throwablesMessages);
		throwablesMessages.throwIfNotEmpty();
		QueryExecutorArguments arguments = findByActorCodeGetQueryExecutorArguments(actorCode, processed, granted, pageable, firstTupleIndex, numberOfTuples);
		return EntityReader.getInstance().readMany(getEntityClass(), arguments);
	}
	
	protected QueryExecutorArguments findByActorCodeGetQueryExecutorArguments(String actorCode, Boolean processed, Boolean granted, Boolean pageable,Integer firstTupleIndex, Integer numberOfTuples) {
		QueryExecutorArguments arguments = new QueryExecutorArguments();
		arguments.setQuery(new Query().setIdentifier(QueryIdentifierBuilder.getInstance().build(getEntityClass(), QueryName.READ_DYNAMIC)));
		arguments.addProjectionsFromStrings(AbstractActorRequest.FIELD_IDENTIFIER,AbstractActorRequest.FIELD_COMMENT,AbstractActorRequest.FIELD_PROCESSING_COMMENT);
		arguments.addFilterField(AbstractActorRequestQuerier.PARAMETER_NAME_ACTORS_CODES,List.of(actorCode));
		if(processed != null) {
			arguments.addFilterField(AbstractActorRequestQuerier.PARAMETER_NAME_PROCESSED, processed);
			arguments.addFilterField(AbstractActorRequestQuerier.PARAMETER_NAME_GRANTED, granted);
			if(Boolean.TRUE.equals(pageable)) {
				arguments.setFirstTupleIndex(firstTupleIndex);
				arguments.setNumberOfTuples(numberOfTuples);
			}
		}
		return arguments;
	}
}