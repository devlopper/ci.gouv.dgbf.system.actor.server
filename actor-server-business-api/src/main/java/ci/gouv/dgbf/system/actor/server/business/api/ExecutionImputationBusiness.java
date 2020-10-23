package ci.gouv.dgbf.system.actor.server.business.api;

import java.util.Collection;

import javax.transaction.Transactional;

import org.cyk.utility.__kernel__.business.TransactionResult;
import org.cyk.utility.__kernel__.persistence.query.filter.Filter;
import org.cyk.utility.server.business.BusinessEntity;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

public interface ExecutionImputationBusiness extends BusinessEntity<ExecutionImputation> {

	String SAVE_SCOPE_FUNCTIONS = "ExecutionImputation.saveScopeFunctions";
	@Transactional
	TransactionResult saveScopeFunctions(Collection<ExecutionImputation> executionImputations);
	
	String DERIVE_FROM_SCOPE_FUNCTIONS = "ExecutionImputation.deriveFromScopeFunctions";
	@Transactional
	TransactionResult deriveFromScopeFunctions(Collection<ScopeFunction> scopeFunctions);
	
	String DERIVE_FROM_ALL_SCOPE_FUNCTIONS = "ExecutionImputation.deriveFromAllScopeFunctions";
	TransactionResult deriveFromAllScopeFunctions();
	
	String DERIVE_SCOPE_FUNCTIONS_FROM_MODEL = "ExecutionImputation.deriveScopeFunctionsFromModel";
	TransactionResult deriveScopeFunctionsFromModel(ExecutionImputation executionImputation,Filter filter);
	
	String DELETE_ALL = "ExecutionImputation.deleteAll";
}