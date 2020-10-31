package ci.gouv.dgbf.system.actor.server.business.api;

import java.util.Collection;

import javax.transaction.Transactional;

import org.cyk.utility.__kernel__.object.marker.IdentifiableSystem;
import org.cyk.utility.server.business.BusinessEntity;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunctionExecutionImputation;

public interface ScopeFunctionExecutionImputationBusiness extends BusinessEntity<ScopeFunctionExecutionImputation> {
	
	@Transactional
	void deriveFromExecutionImputations(Collection<ExecutionImputation> executionImputations);
	
	void deriveAll();
	
	static String identify(String scopeFunctionIdentifier,String executionImputationIdentifier) {
		//if(StringHelper.isBlank(scopeFunctionIdentifier) || StringHelper.isBlank(executionImputationIdentifier))
			return IdentifiableSystem.generateRandomly();
		//return scopeFunctionIdentifier+executionImputationIdentifier;
	}
	
	String SAVE = "ScopeFunctionExecutionImputation.save";
	
	String DERIVE_FROM_EXECUTION_IMPUTATIONS = "ScopeFunctionExecutionImputation.deriveFromExecutionImputations";
	String DERIVE_ALL = "ScopeFunctionExecutionImputation.deriveAll";
	
	String DELETE_ALL = "ScopeFunctionExecutionImputation.deleteAll";
}