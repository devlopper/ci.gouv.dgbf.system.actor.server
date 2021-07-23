package ci.gouv.dgbf.system.actor.server.business.api;

import java.util.Collection;

import javax.transaction.Transactional;

import org.cyk.utility.business.TransactionResult;
import org.cyk.utility.server.business.BusinessEntity;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorScope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;

public interface ActorScopeBusiness extends BusinessEntity<ActorScope> {

	@Transactional
	void createByActorByScopes(Actor actor,Collection<Scope> scopes);
	
	@Transactional
	void deleteByActorByScopes(Actor actor,Collection<Scope> scopes);
	
	@Transactional
	void createByActorsByScopes(Collection<Actor> actors,Collection<Scope> scopes);
	
	@Transactional
	void createByActorIdentifierByScopesIdentifier(String actorIdentifier,String...scopesIdentifiers);
	
	@Transactional
	void deleteByActorsByScopes(Collection<Actor> actors,Collection<Scope> scopes);
	
	@Transactional
	void deleteByActorIdentifierByScopesIdentifiers(String actorIdentifier,String...scopesIdentifiers);
	
	String VISIBLE = "ActorScope.visible";
	@Transactional
	TransactionResult visible(Collection<String> actorsIdentifiers,Collection<String> scopesIdentifiers);
	
	String UNVISIBLE = "ActorScope.unvisible";
	@Transactional
	TransactionResult unvisible(Collection<String> actorsIdentifiers,Collection<String> scopesIdentifiers);
	
	String DELETE_BY_ACTORS_IDENTIFIERS_BY_SCOPES_IDENTIFIERS = "ActorScope.deleteByActorsIdentifiersByScopesIdentifiers";
	@Transactional
	void deleteByActorsIdentifiersByScopesIdentifiers(Collection<String> actorsIdentifiers,Collection<String> scopesIdentifiers);
	
	/**/
	
	String CREATE_BY_ACTOR_BY_SCOPES = "ActorScope.createByActorByScopes";
	String CREATE_BY_ACTORS_BY_SCOPES = "ActorScope.createByActorsByScopes";
	String DELETE_BY_ACTOR_BY_SCOPES = "ActorScope.deleteByActorByScopes";
	String DELETE_BY_ACTORS_BY_SCOPES = "ActorScope.deleteByActorsByScopes";
	
}