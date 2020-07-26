package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.ActorFunctionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorFunction;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class ActorFunctionPersistenceImpl extends AbstractPersistenceEntityImpl<ActorFunction> implements ActorFunctionPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}