package ci.gouv.dgbf.system.actor.server.business.api;

import java.util.Collection;

import javax.transaction.Transactional;

import org.cyk.utility.server.business.BusinessEntity;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorScope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;

public interface ActorScopeBusiness extends BusinessEntity<ActorScope> {

	@Transactional
	void deleteByActorByScopes(Actor actor,Collection<Scope> scopes);
	
	/**/
	
	String DELETE_BY_ACTOR_BY_SCOPES = "ActorScope.deleteByActorByScopes";
}