package ci.gouv.dgbf.system.actor.server.business.api;

import java.util.Collection;

import javax.transaction.Transactional;

import org.cyk.utility.server.business.BusinessEntity;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

public interface ScopeFunctionBusiness extends BusinessEntity<ScopeFunction> {

	@Transactional
	void deriveFromScopesFromFunctions(Collection<Scope> scopes,Collection<Function> functions);
	void deriveAll();
	
	@Transactional
	void codify(Collection<ScopeFunction> scopeFunctions);
	void codifyAll();
	
	String SAVE = "ScopeFunction.save";
	
	String DERIVE_FROM_SCOPES_FROM_FUNCTIONS = "ScopeFunction.deriveFromScopesFromFunctions";
	String DERIVE_ALL = "ScopeFunction.deriveAll";
	
	String CODIFY = "ScopeFunction.codify";
	String CODIFY_ALL = "ScopeFunction.codifyAll";
	
	String DELETE_ALL = "ScopeFunction.deleteAll";	
}