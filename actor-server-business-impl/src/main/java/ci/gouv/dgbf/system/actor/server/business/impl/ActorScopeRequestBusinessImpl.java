package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.enterprise.context.ApplicationScoped;
import javax.persistence.EntityManager;
import javax.transaction.Transactional;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.map.MapHelper;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.throwable.ThrowablesMessages;
import org.cyk.utility.business.TransactionResult;
import org.cyk.utility.business.server.AbstractSpecificBusinessImpl;
import org.cyk.utility.business.server.EntityCreator;
import org.cyk.utility.business.server.EntityDeletor;
import org.cyk.utility.business.server.EntityUpdater;
import org.cyk.utility.persistence.EntityManagerGetter;
import org.cyk.utility.persistence.query.EntityFinder;
import org.cyk.utility.persistence.query.QueryExecutorArguments;

import ci.gouv.dgbf.system.actor.server.business.api.ActorScopeRequestBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorScopeRequest;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;

@ApplicationScoped
public class ActorScopeRequestBusinessImpl extends AbstractSpecificBusinessImpl<ActorScopeRequest> implements ActorScopeRequestBusiness,Serializable {
	private static final long serialVersionUID = 1L;

	public static TransactionResult record(Collection<String> actorsIdentifiers, Collection<String> scopesIdentifiers,String actorCode,Boolean ignoreExisting,EntityManager entityManager) {
		if(CollectionHelper.isEmpty(actorsIdentifiers))
			throw new RuntimeException("Identifiants acteurs requis");
		if(CollectionHelper.isEmpty(scopesIdentifiers))
			throw new RuntimeException("Identifiants domaines requis");
		if(StringHelper.isBlank(actorCode))
			throw new RuntimeException("Nom utilisateur requis");
		@SuppressWarnings("unchecked")
		List<Object[]> arrays = entityManager.createQuery("SELECT t.actor.identifier,t.scope.identifier,t.actor.code,t.scope.code FROM ActorScopeRequest t WHERE t.actor.identifier IN :actorsIdentifiers"
				+ " AND t.scope.identifier IN :scopesIdentifiers").setParameter("actorsIdentifiers", actorsIdentifiers).setParameter("scopesIdentifiers", scopesIdentifiers)
				.getResultList();
		if(CollectionHelper.isNotEmpty(arrays))
			throw new RuntimeException(String.format("Il existe %s demande(s) en cours de validation. <<%s>>",arrays.size()
					,arrays.stream().map(a -> a[2]+","+a[3]).collect(Collectors.joining("|"))));
		Collection<Actor> actors = EntityFinder.getInstance().findMany(Actor.class, actorsIdentifiers);
		if(CollectionHelper.isEmpty(actors))
			throw new RuntimeException("acteurs requis");
		Collection<Scope> scopes = EntityFinder.getInstance().findMany(Scope.class, scopesIdentifiers);
		if(CollectionHelper.isEmpty(scopes))
			throw new RuntimeException("domaines requis");
		Collection<Object> requests = new ArrayList<Object>();
		actors.forEach(actor -> {
			scopes.forEach(scope -> {
				ActorScopeRequest actorScopeRequest = new ActorScopeRequest();
				actorScopeRequest.setActor(actor).setScope(scope).set__auditFunctionality__("Demande de domaine").set__auditWho__(actorCode);
				requests.add(actorScopeRequest);
			});
		});
		TransactionResult transactionResult = new TransactionResult().setName(String.format("Demande de domaines %s",requests)).setTupleName("Demande domaine");
		EntityCreator.getInstance().create(new QueryExecutorArguments().setObjects(requests).setEntityManager(entityManager));
		transactionResult.incrementNumberOfCreation(Long.valueOf(requests.size()));
		transactionResult.log(ActorScopeRequestBusinessImpl.class);
		return transactionResult;
	}
	
	@Override @Transactional
	public TransactionResult record(Collection<String> actorsIdentifiers, Collection<String> scopesIdentifiers,String actorCode,Boolean ignoreExisting) {
		return record(actorsIdentifiers, scopesIdentifiers, actorCode,ignoreExisting, EntityManagerGetter.getInstance().get());
	}
	
	/**/
	
	public static TransactionResult cancel(Collection<String> identifiers, String actorCode,Boolean ignoreExisting,EntityManager entityManager) {
		if(CollectionHelper.isEmpty(identifiers))
			throw new RuntimeException("Identifiants requis");
		if(StringHelper.isBlank(actorCode))
			throw new RuntimeException("Nom utilisateur requis");		
		Collection<ActorScopeRequest> actorScopeRequests = EntityManagerGetter.getInstance().get()
				.createQuery("SELECT t FROM ActorScopeRequest t WHERE t.identifier IN :identifiers AND t.granted IS NULL", ActorScopeRequest.class)
				.setParameter("identifiers", identifiers).getResultList();
		//EntityFinder.getInstance().findMany(ActorScopeRequest.class, identifiers);
		if(CollectionHelper.isEmpty(actorScopeRequests))
			throw new RuntimeException("domaines à annuler requis");
		actorScopeRequests.forEach(actorScopeRequest -> {
			actorScopeRequest.set__auditFunctionality__("Annulation de demande de domaine").set__auditWho__(actorCode);
		});
		TransactionResult transactionResult = new TransactionResult().setName(String.format("Annulation de demande de domaines %s",identifiers)).setTupleName("Demande domaine annulée");
		EntityDeletor.getInstance().delete(new QueryExecutorArguments().setObjects(CollectionHelper.cast(Object.class, actorScopeRequests)).setEntityManager(entityManager));
		transactionResult.incrementNumberOfCreation(Long.valueOf(actorScopeRequests.size()));
		transactionResult.log(ActorScopeRequestBusinessImpl.class);
		return transactionResult;
	}
	
	@Override @Transactional
	public TransactionResult cancel(Collection<String> identifiers, String actorCode,Boolean ignoreExisting) {
		return cancel(identifiers, actorCode,ignoreExisting, EntityManagerGetter.getInstance().get());
	}
	
	/**/
	
	public static TransactionResult process(Collection<ActorScopeRequest> actorScopeRequests, String actorCode,EntityManager entityManager) {
		ThrowablesMessages throwablesMessages = new ThrowablesMessages();
		actorScopeRequests.forEach(actorScopeRequest -> {
			if(actorScopeRequest.getGranted() == null)
				throwablesMessages.add(String.format("Spécifier explicitement la valeur accordée de %s", actorScopeRequest.getIdentifier()));
			if(Boolean.FALSE.equals(actorScopeRequest.getGranted()) && StringHelper.isBlank(actorScopeRequest.getProcessingComment()))
				throwablesMessages.add(String.format("Spécifier le motif de rejet de %s", actorScopeRequest.getIdentifier()));
			actorScopeRequest.set__auditFunctionality__("Traitement de demande de domaine").set__auditWho__(actorCode);
		});
		throwablesMessages.throwIfNotEmpty();
		TransactionResult transactionResult = new TransactionResult().setName(String.format("Traitement de demande de domaines")).setTupleName("Demande domaine traitée");
		EntityUpdater.getInstance().update(new QueryExecutorArguments().setObjects(CollectionHelper.cast(Object.class, actorScopeRequests)).setEntityManager(entityManager));
		transactionResult.incrementNumberOfCreation(Long.valueOf(actorScopeRequests.size()));
		transactionResult.log(ActorScopeRequestBusinessImpl.class);
		
		for(ActorScopeRequest actorScopeRequest : actorScopeRequests) {
			if(Boolean.TRUE.equals(actorScopeRequest.getGranted())) {
				ActorScopeBusinessImpl.visible(List.of(actorScopeRequest.getActor().getIdentifier()), List.of(actorScopeRequest.getScope().getIdentifier())
						, Boolean.TRUE, actorCode,entityManager);
			}
		}
		
		return transactionResult;
	}
	
	public static TransactionResult process(Collection<String> identifiers,Map<String,Boolean> grants,Map<String,String> comments, String actorCode,EntityManager entityManager) {
		if(CollectionHelper.isEmpty(identifiers))
			throw new RuntimeException("Identifiants requis");
		if(StringHelper.isBlank(actorCode))
			throw new RuntimeException("Nom utilisateur requis");
		Collection<ActorScopeRequest> actorScopeRequests = EntityFinder.getInstance().findMany(ActorScopeRequest.class, identifiers);
		if(CollectionHelper.isEmpty(actorScopeRequests))
			throw new RuntimeException("domaines à annuler requis");
		actorScopeRequests.forEach(actorScopeRequest -> {
			actorScopeRequest.setGranted(MapHelper.readByKey(grants, actorScopeRequest.getIdentifier()));
			actorScopeRequest.setComment(MapHelper.readByKey(comments, actorScopeRequest.getIdentifier()));
		});
		return process(actorScopeRequests, actorCode, entityManager);
	}
	
	@Override @Transactional
	public TransactionResult process(Collection<String> identifiers,Map<String,Boolean> grants,Map<String,String> comments, String actorCode) {
		return process(identifiers,grants,comments,actorCode, EntityManagerGetter.getInstance().get());
	}
	
	@Override @Transactional
	public TransactionResult process(Collection<ActorScopeRequest> actorScopeRequests, String actorCode) {
		return process(actorScopeRequests, actorCode, EntityManagerGetter.getInstance().get());
	}
}