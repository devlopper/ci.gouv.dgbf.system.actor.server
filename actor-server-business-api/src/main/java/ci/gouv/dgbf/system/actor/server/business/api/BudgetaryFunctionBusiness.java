package ci.gouv.dgbf.system.actor.server.business.api;

import ci.gouv.dgbf.system.actor.server.persistence.entities.BudgetaryFunction;

import javax.transaction.Transactional;

import org.cyk.utility.server.business.BusinessEntity;

public interface BudgetaryFunctionBusiness extends BusinessEntity<BudgetaryFunction> {

	@Transactional
	void generate();
	
	String GENERATE = "BudgetaryFunction.generate";
	String DELETE_ALL = "BudgetaryFunction.deleteAll";
}