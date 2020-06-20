package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.AccountPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Account;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class AccountPersistenceImpl extends AbstractPersistenceEntityImpl<Account> implements AccountPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}