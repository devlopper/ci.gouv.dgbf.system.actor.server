package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.ProfilePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class ProfilePersistenceImpl extends AbstractPersistenceEntityImpl<Profile> implements ProfilePersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}