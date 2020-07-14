package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.AccountRequestPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AccountRequest;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class AccountRequestPersistenceImpl extends AbstractPersistenceEntityImpl<AccountRequest> implements AccountRequestPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}