package ci.gouv.dgbf.system.actor.server.business.api;

import java.util.Collection;

import javax.transaction.Transactional;

import org.cyk.utility.server.business.BusinessEntity;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

public interface ScopeFunctionBusiness extends BusinessEntity<ScopeFunction> {

	String DERIVE_ALL = "ScopeFunction.deriveAll";
	void deriveAll();
	String DERIVE_BY_FUNCTIONS_IDENTIFIERS = "ScopeFunction.deriveByFunctionsIdentifiers";
	void deriveByFunctionsIdentifiers(Collection<String> functionsIdentifiers);
	
	String CODIFY = "ScopeFunction.codify";
	@Transactional
	void codify(Collection<ScopeFunction> scopeFunctions);
	
	String CODIFY_ALL = "ScopeFunction.codifyAll";
	void codifyAll();
	String CODIFY_BY_FUNCTIONS_IDENTIFIERS = "ScopeFunction.codifyByFunctionsIdentifiers";
	void codifyByFunctionsIdentifiers(Collection<String> functionsIdentifiers);
	
	String DELETE_BY_FUNCTIONS_IDENTIFIERS = "ScopeFunction.deleteByFunctionsIdentifiers";
	void deleteByFunctionsIdentifiers(Collection<String> functionsIdentifiers);
	
	String SAVE = "ScopeFunction.save";
		
	String DELETE_ALL = "ScopeFunction.deleteAll";	
}