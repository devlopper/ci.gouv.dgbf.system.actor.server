package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.BudgetaryFunctionBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.BudgetaryFunctionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.BudgetaryFunction;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class BudgetaryFunctionBusinessImpl extends AbstractBusinessEntityImpl<BudgetaryFunction, BudgetaryFunctionPersistence> implements BudgetaryFunctionBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
