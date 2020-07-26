package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.AccountRequestFunctionBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.AccountRequestFunctionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AccountRequestFunction;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class AccountRequestFunctionBusinessImpl extends AbstractBusinessEntityImpl<AccountRequestFunction, AccountRequestFunctionPersistence> implements AccountRequestFunctionBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
