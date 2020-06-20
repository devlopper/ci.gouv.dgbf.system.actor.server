package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.ProfilePrivilegePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfilePrivilege;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class ProfilePrivilegePersistenceImpl extends AbstractPersistenceEntityImpl<ProfilePrivilege> implements ProfilePrivilegePersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}