package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.AccountBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.AccountPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Account;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class AccountBusinessImpl extends AbstractBusinessEntityImpl<Account, AccountPersistence> implements AccountBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
