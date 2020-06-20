package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.ActorPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class ActorPersistenceImpl extends AbstractPersistenceEntityImpl<Actor> implements ActorPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}