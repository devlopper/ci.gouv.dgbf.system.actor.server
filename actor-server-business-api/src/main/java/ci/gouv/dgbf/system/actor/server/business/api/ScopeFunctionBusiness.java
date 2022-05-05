package ci.gouv.dgbf.system.actor.server.business.api;

import java.util.Collection;

import javax.transaction.Transactional;

import org.cyk.utility.business.TransactionResult;
import org.cyk.utility.server.business.BusinessEntity;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

public interface ScopeFunctionBusiness extends BusinessEntity<ScopeFunction> {

	String CREATE_BY_SCOPE_IDENTIFIER_BY_CATEGORY_CODE = "ScopeFunction.createByScopeIdentifierByCategoryCode";
	@Transactional
	TransactionResult createByScopeIdentifierByCategoryCode(String scopeIdentifier,String categoryCode,String name,String actorCode,Boolean throwOnExisting);
	
	String CREATE_MANY = "ScopeFunction.createMany";
	
	String DERIVE_ALL = "ScopeFunction.deriveAll";
	void deriveAll();
	String DERIVE_BY_FUNCTIONS = "ScopeFunction.deriveByFunctions";
	void deriveByFunctions(Collection<Function> functions);
	String DERIVE_BY_FUNCTIONS_IDENTIFIERS = "ScopeFunction.deriveByFunctionsIdentifiers";
	void deriveByFunctionsIdentifiers(Collection<String> functionsIdentifiers);	
	String DERIVE_HOLDERS_AND_ASSISTANTS_BY_HOLDERS_FUNCTIONS_IDENTIFIERS = "ScopeFunction.deriveHoldersAndAssistantsByHoldersFunctionsIdentifiers";
	void deriveHoldersAndAssistantsByHoldersFunctionsIdentifiers(Collection<String> holdersFunctionsIdentifiers);
	
	String CODIFY = "ScopeFunction.codify";
	@Transactional
	void codify(Collection<ScopeFunction> scopeFunctions);	
	String CODIFY_ALL = "ScopeFunction.codifyAll";
	void codifyAll();
	String CODIFY_BY_FUNCTIONS = "ScopeFunction.codifyByFunctions";
	void codifyByFunctions(Collection<Function> functions);
	String CODIFY_BY_FUNCTIONS_IDENTIFIERS = "ScopeFunction.codifyByFunctionsIdentifiers";
	void codifyByFunctionsIdentifiers(Collection<String> functionsIdentifiers);
	String CODIFY_HOLDERS_AND_ASSISTANTS_BY_HOLDERS_FUNCTIONS_IDENTIFIERS = "ScopeFunction.codifyHoldersAndAssistantsByHoldersFunctionsIdentifiers";
	void codifyHoldersAndAssistantsByHoldersFunctionsIdentifiers(Collection<String> holdersFunctionsIdentifiers);
	
	String SAVE_ASSISTANTS = "ScopeFunction.saveAssistants";
	@Transactional
	void saveAssistants(Collection<ScopeFunction> scopeFunctions);	
	
	String DELETE_BY_FUNCTIONS = "ScopeFunction.deleteByFunctions";
	void deleteByFunctions(Collection<Function> functions);
	String DELETE_BY_FUNCTIONS_IDENTIFIERS = "ScopeFunction.deleteByFunctionsIdentifiers";
	void deleteByFunctionsIdentifiers(Collection<String> functionsIdentifiers);
	String DELETE_HOLDERS_AND_ASSISTANTS_BY_HOLDERS_FUNCTIONS_IDENTIFIERS = "ScopeFunction.deleteHoldersAndAssistantsByHoldersFunctionsIdentifiers";
	void deleteHoldersAndAssistantsByHoldersFunctionsIdentifiers(Collection<String> holdersFunctionsIdentifiers);
	
	void updateActorsCodesFromExternal();
	
	@Transactional
	void export(String actorCode);
	
	@Transactional
	void exportAsynchronously(String actorCode);
	
	String SAVE = "ScopeFunction.save";
	String SAVE_NAME = "ScopeFunction.saveName";

	String DELETE_ALL = "ScopeFunction.deleteAll";	
}