package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.UserAccountProfilePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.UserAccountProfile;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class UserAccountProfilePersistenceImpl extends AbstractPersistenceEntityImpl<UserAccountProfile> implements UserAccountProfilePersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}