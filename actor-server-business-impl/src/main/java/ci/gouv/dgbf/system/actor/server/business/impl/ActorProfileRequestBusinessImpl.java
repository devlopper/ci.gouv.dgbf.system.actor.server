package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.util.Collection;
import java.util.Map;

import javax.enterprise.context.ApplicationScoped;
import javax.persistence.EntityManager;

import org.cyk.utility.business.TransactionResult;

import ci.gouv.dgbf.system.actor.server.business.api.ActorProfileRequestBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorProfile;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorProfileRequest;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;

@ApplicationScoped
public class ActorProfileRequestBusinessImpl extends AbstractActorRequestBusinessImpl<ActorProfileRequest> implements ActorProfileRequestBusiness,Serializable {
	private static final long serialVersionUID = 1L;

	public static TransactionResult record(Collection<String> actorsIdentifiers, Collection<String> requestablesIdentifiers,String actorCode,Boolean ignoreExisting,EntityManager entityManager) {
		return record(ActorProfileRequestBusinessImpl.class,ActorProfileRequest.class, actorsIdentifiers, Profile.class, requestablesIdentifiers, actorCode, ignoreExisting, entityManager);
	}
	
	public static TransactionResult cancel(Collection<String> identifiers, String actorCode,Boolean ignoreExisting,EntityManager entityManager) {
		return cancel(ActorProfileRequestBusinessImpl.class,ActorProfileRequest.class, identifiers, actorCode, ignoreExisting, entityManager);
	}
	
	public static TransactionResult process(Collection<ActorProfileRequest> actorProfileRequests, String actorCode,EntityManager entityManager) {
		return process(ActorProfileRequestBusinessImpl.class, actorProfileRequests, actorCode,PROCESS, new ProcessingImpl(), entityManager);
	}
	
	public static TransactionResult process(Collection<String> identifiers,Map<String,Boolean> grants,Map<String,String> comments, String actorCode,EntityManager entityManager) {
		return process(ActorProfileRequestBusinessImpl.class, ActorProfileRequest.class, identifiers, grants, comments, actorCode,PROCESS, new ProcessingImpl(), entityManager);
	}
	
	@Override
	protected String getProcessActionIdentitifer() {
		return PROCESS;
	}
	
	@Override
	protected Processing<ActorProfileRequest> getPocessing() {
		return new ProcessingImpl();
	}
	
	@Override
	protected Class<?> getRequestableClass() {
		return Profile.class;
	}
	
	@Override
	protected Class<ActorProfileRequest> getEntityClass() {
		return ActorProfileRequest.class;
	}
	
	/**/
	
	public static class ProcessingImpl implements Processing<ActorProfileRequest> {

		@Override
		public void listenGrantIsTrue(ActorProfileRequest request,EntityManager entityManager) {
			ActorProfileBusinessImpl.create(new ActorProfile().setActor(request.getActor()).setProfile(request.getProfile()), entityManager);
		}

		@Override
		public void listenGrantIsNotTrue(ActorProfileRequest request,EntityManager entityManager) {
			// Nothing to do
		}
	}
}