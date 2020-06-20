package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.UserPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.User;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class UserPersistenceImpl extends AbstractPersistenceEntityImpl<User> implements UserPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}