package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.ActorProfilePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorProfile;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class ActorProfilePersistenceImpl extends AbstractPersistenceEntityImpl<ActorProfile> implements ActorProfilePersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}