package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.ActorProfileRequestPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorProfileRequest;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class ActorProfileRequestPersistenceImpl extends AbstractPersistenceEntityImpl<ActorProfileRequest> implements ActorProfileRequestPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}