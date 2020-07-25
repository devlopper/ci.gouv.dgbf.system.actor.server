package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.AccountRequestBudgetaryFunctionBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.AccountRequestBudgetaryFunctionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AccountRequestBudgetaryFunction;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class AccountRequestBudgetaryFunctionBusinessImpl extends AbstractBusinessEntityImpl<AccountRequestBudgetaryFunction, AccountRequestBudgetaryFunctionPersistence> implements AccountRequestBudgetaryFunctionBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
