package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.ActorScopePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorScope;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class ActorScopePersistenceImpl extends AbstractPersistenceEntityImpl<ActorScope> implements ActorScopePersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}