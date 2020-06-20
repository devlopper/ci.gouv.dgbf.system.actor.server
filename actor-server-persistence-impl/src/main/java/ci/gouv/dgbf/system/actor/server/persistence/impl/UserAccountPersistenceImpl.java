package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.UserAccountPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.UserAccount;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class UserAccountPersistenceImpl extends AbstractPersistenceEntityImpl<UserAccount> implements UserAccountPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}