package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.AccountingServicePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AccountingService;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class AccountingServicePersistenceImpl extends AbstractPersistenceEntityImpl<AccountingService> implements AccountingServicePersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}