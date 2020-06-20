package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.ProfileTypePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileType;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class ProfileTypePersistenceImpl extends AbstractPersistenceEntityImpl<ProfileType> implements ProfileTypePersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}