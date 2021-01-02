package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.AccountingServiceBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.AccountingServicePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AccountingService;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class AccountingServiceBusinessImpl extends AbstractBusinessEntityImpl<AccountingService, AccountingServicePersistence> implements AccountingServiceBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
