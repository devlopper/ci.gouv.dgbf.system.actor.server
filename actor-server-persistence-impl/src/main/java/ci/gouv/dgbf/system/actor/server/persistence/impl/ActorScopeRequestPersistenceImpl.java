package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.ActorScopeRequestPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorScopeRequest;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class ActorScopeRequestPersistenceImpl extends AbstractPersistenceEntityImpl<ActorScopeRequest> implements ActorScopeRequestPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}