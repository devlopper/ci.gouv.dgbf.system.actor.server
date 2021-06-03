package ci.gouv.dgbf.system.actor.server.business.api;

import java.util.Collection;

import javax.transaction.Transactional;

import org.cyk.utility.business.TransactionResult;
import org.cyk.utility.server.business.BusinessEntity;

import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestScopeFunction;

public interface RequestScopeFunctionBusiness extends BusinessEntity<RequestScopeFunction> {

	String UPDATE_GRANTED_TO_FALSE_WHERE_TRUE_BY_SCOPE_FUNCTIONS_IDENTIFIERS = "RequestScopeFunction.updateGrantedToFalseWhereTrueByScopeFunctionsIdentifiers";
	@Transactional
	TransactionResult updateGrantedToFalseWhereTrueByScopeFunctionsIdentifiers(Collection<String> scopeFunctionsIdentifiers,String actorCode);
	
	@Transactional
	TransactionResult updateGrantedToFalseWhereTrueByScopeFunctionsIdentifiers(String actorCode,String...scopeFunctionsIdentifiers);
}