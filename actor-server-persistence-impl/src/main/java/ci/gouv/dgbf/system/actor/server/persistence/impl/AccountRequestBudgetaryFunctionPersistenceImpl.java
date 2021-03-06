package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.AccountRequestBudgetaryFunctionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AccountRequestBudgetaryFunction;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class AccountRequestBudgetaryFunctionPersistenceImpl extends AbstractPersistenceEntityImpl<AccountRequestBudgetaryFunction> implements AccountRequestBudgetaryFunctionPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}