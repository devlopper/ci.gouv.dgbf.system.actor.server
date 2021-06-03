package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import static org.cyk.utility.persistence.query.Language.jpql;

import java.time.LocalDateTime;
import java.util.Collection;

import javax.persistence.EntityManager;

import org.cyk.utility.__kernel__.array.ArrayHelper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutor;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.persistence.query.QueryManager;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestScopeFunction;

public class RequestScopeFunctionQuerierImpl extends RequestScopeFunctionQuerier.AbstractImpl {

	@Override
	public Integer updateGrantedToFalseWhereTrueByScopeFunctionsIdentifiers(Collection<String> scopeFunctionsIdentifiers,String auditActor,String auditFunctionality,String auditAction,LocalDateTime auditDate,EntityManager entityManager) {
		if(CollectionHelper.isEmpty(scopeFunctionsIdentifiers))
			return null;
		QueryExecutorArguments arguments = new QueryExecutorArguments().setQueryFromIdentifier(QUERY_IDENTIFIER_UPDATE_GRANTED_TO_FALSE_WHERE_TRUE_BY_SCOPE_FUNCTION_IDENTIFIERS);
		arguments.addFilterField(PARAMETER_NAME_SCOPE_FUNCTIONS_IDENTIFIERS, scopeFunctionsIdentifiers);
		arguments.addFilterField(PARAMETER_NAME_AUDIT_WHO, auditActor);
		arguments.addFilterField(PARAMETER_NAME_AUDIT_FUNCTIONALITY, auditFunctionality);
		arguments.addFilterField(PARAMETER_NAME_AUDIT_WHAT, auditAction);
		arguments.addFilterField(PARAMETER_NAME_AUDIT_WHEN, auditDate);
		arguments.setEntityManager(entityManager);
		return QueryExecutor.getInstance().executeUpdateOrDelete(arguments);
	}
	
	@Override
	public Integer updateGrantedToFalseWhereTrueByScopeFunctionsIdentifiers(EntityManager entityManager,String auditActor,String auditFunctionality,String auditAction,LocalDateTime auditDate,String...scopeFunctionsIdentifiers) {
		if(ArrayHelper.isEmpty(scopeFunctionsIdentifiers))
			return null;
		return updateGrantedToFalseWhereTrueByScopeFunctionsIdentifiers(CollectionHelper.listOf(scopeFunctionsIdentifiers),auditActor,auditFunctionality,auditAction,auditDate,entityManager);
	}
	
	public static void initialize() {
		QueryManager.getInstance().register(
				Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_UPDATE_GRANTED_TO_FALSE_WHERE_TRUE_BY_SCOPE_FUNCTION_IDENTIFIERS
					,Query.FIELD_VALUE,jpql(
							"UPDATE RequestScopeFunction t"
							,"SET t."+RequestScopeFunction.FIELD_GRANTED+" = false"
							,",t."+RequestScopeFunction.FIELD___AUDIT_WHO__+" = :"+PARAMETER_NAME_AUDIT_WHO
							,",t."+RequestScopeFunction.FIELD___AUDIT_FUNCTIONALITY__+" = :"+PARAMETER_NAME_AUDIT_FUNCTIONALITY
							,",t."+RequestScopeFunction.FIELD___AUDIT_WHAT__+" = :"+PARAMETER_NAME_AUDIT_WHAT
							,",t."+RequestScopeFunction.FIELD___AUDIT_WHEN__+" = :"+PARAMETER_NAME_AUDIT_WHEN
							,"WHERE t."+RequestScopeFunction.FIELD_GRANTED+" = true AND t.scopeFunction.identifier IN :"+PARAMETER_NAME_SCOPE_FUNCTIONS_IDENTIFIERS
							)));
	}

	public static final String QUERY_IDENTIFIER_UPDATE_GRANTED_TO_FALSE_WHERE_TRUE_BY_SCOPE_FUNCTION_IDENTIFIERS
		= QueryIdentifierBuilder.getInstance().build(RequestScopeFunction.class, "updateGrantedToFalseByScopeFunctionsIdentifiers");
}