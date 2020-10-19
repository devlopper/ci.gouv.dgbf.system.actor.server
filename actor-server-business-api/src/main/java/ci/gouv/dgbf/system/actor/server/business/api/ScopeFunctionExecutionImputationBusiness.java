package ci.gouv.dgbf.system.actor.server.business.api;

import java.util.Collection;

import javax.transaction.Transactional;

import org.cyk.utility.server.business.BusinessEntity;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunctionExecutionImputation;

public interface ScopeFunctionExecutionImputationBusiness extends BusinessEntity<ScopeFunctionExecutionImputation> {
	
	@Transactional
	void deriveFromScopesFromFunctions(Collection<ScopeFunction> scopeFunctions,Collection<ExecutionImputation> functionImputations);
	void deriveAll();
	
	String SAVE = "ScopeFunctionExecutionImputation.save";
	
	String DERIVE_FROM_SCOPES_FROM_FUNCTIONS = "ScopeFunctionExecutionImputation.deriveFromScopesFromFunctions";
	String DERIVE_ALL = "ScopeFunctionExecutionImputation.deriveAll";
	
	String DELETE_ALL = "ScopeFunctionExecutionImputation.deleteAll";
}