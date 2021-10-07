package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import static org.cyk.utility.persistence.query.Language.jpql;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Collection;

import javax.persistence.EntityManager;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.value.Value;
import org.cyk.utility.persistence.EntityManagerGetter;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutor;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.persistence.query.QueryManager;
import org.cyk.utility.persistence.query.QueryName;
import org.cyk.utility.persistence.server.query.executor.DynamicManyExecutor;
import org.cyk.utility.persistence.server.query.executor.DynamicOneExecutor;

import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestScopeFunction;

public interface RequestScopeFunctionQuerier extends Querier {

	String PARAMETER_NAME_REQUESTS_IDENTIFIERS = "requestsIdentifiers";
	String PARAMETER_NAME_REQUEST_IDENTIFIER = "requestIdentifier";
	String PARAMETER_NAME_SCOPE_FUNCTIONS_IDENTIFIERS = "scopeFunctionsIdentifiers";
	String PARAMETER_NAME_SCOPE_FUNCTION_IDENTIFIER = "scopeFunctionIdentifier";
	
	String QUERY_IDENTIFIER_READ_BY_REQUESTS_IDENTIFIER_BY_SCOPE_FUNCTIONS_IDENTIFIER = QueryIdentifierBuilder.getInstance().build(RequestScopeFunction.class
			, "readByRequestIdentifierByScopeFunctionIdentifier");
	RequestScopeFunction readByRequestIdentifierByScopeFunctionIdentifier(String requestIdentifier,String scopeFunctionIdentifier);
	
	String QUERY_IDENTIFIER_READ_BY_REQUESTS_IDENTIFIERS = QueryIdentifierBuilder.getInstance().build(RequestScopeFunction.class, "readByRequestsIdentifiers");
	Collection<RequestScopeFunction> readByRequestsIdentifiers(Collection<String> requestsIdentifiers);
	
	String QUERY_IDENTIFIER_READ_USING_SCALAR_MODE_BY_REQUESTS_IDENTIFIERS = QueryIdentifierBuilder.getInstance().build(RequestScopeFunction.class, "readUsingScalarModeByRequestsIdentifiers");
	Collection<RequestScopeFunction> readUsingScalarModeByRequestsIdentifiers(Collection<String> requestsIdentifiers);
	
	String QUERY_IDENTIFIER_READ_WHERE_GRANTED_IS_TRUE_BY_REQUESTS_IDENTIFIERS = QueryIdentifierBuilder.getInstance().build(RequestScopeFunction.class, "readWhereGrantedIsTrueByRequestsIdentifiers");
	Collection<RequestScopeFunction> readWhereGrantedIsTrueByRequestsIdentifiers(Collection<String> requestsIdentifiers);
	
	String QUERY_IDENTIFIER_COUNT_WHERE_GRANTED_IS_TRUE_GROUP_BY_REQUEST_IDENTIFIER_BY_REQUESTS_IDENTIFIERS = QueryIdentifierBuilder.getInstance().build(RequestScopeFunction.class, "countWhereGrantedIsTrueGroupByRequestIdentifierByRequestsIdentifiers");
	Collection<Object[]> countWhereGrantedIsTrueGroupByRequestIdentifierByRequestsIdentifiers(Collection<String> requestsIdentifiers);
	
	String QUERY_IDENTIFIER_COUNT_WHERE_REQUESTED_IS_TRUE_GROUP_BY_SCOPE_FUNCTION_IDENTIFIER_BY_SCOPE_FUNCTIONS_IDENTIFIERS = QueryIdentifierBuilder.getInstance().build(RequestScopeFunction.class, "countWhereRequestedIsTrueGroupByScopeFunctionIdentifierByScopeFunctionsIdentifiers");
	Collection<Object[]> countWhereRequestedIsTrueGroupByScopeFunctionIdentifierByScopeFunctionsIdentifiers(Collection<String> scopeFunctionsIdentifiers);
	
	String QUERY_IDENTIFIER_COUNT_WHERE_GRANTED_IS_TRUE_GROUP_BY_SCOPE_FUNCTION_IDENTIFIER_BY_SCOPE_FUNCTIONS_IDENTIFIERS = QueryIdentifierBuilder.getInstance().build(RequestScopeFunction.class, "countWhereGrantedIsTrueGroupByScopeFunctionIdentifierByScopeFunctionsIdentifiers");
	Collection<Object[]> countWhereGrantedIsTrueGroupByScopeFunctionIdentifierByScopeFunctionsIdentifiers(Collection<String> scopeFunctionsIdentifiers);
	
	String QUERY_IDENTIFIER_READ_BY_SCOPE_FUNCTIONS_IDENTIFIERS = QueryIdentifierBuilder.getInstance().build(RequestScopeFunction.class, "readByScopeFunctionsIdentifiers");
	Collection<RequestScopeFunction> readByScopeFunctionsIdentifiers(Collection<String> scopeFunctionsIdentifiers);
	
	String QUERY_IDENTIFIER_READ_WHERE_GRANTED_IS_TRUE_BY_SCOPE_FUNCTIONS_IDENTIFIERS = QueryIdentifierBuilder.getInstance().build(RequestScopeFunction.class, "readWhereGrantedIsTrueByScopeFunctionsIdentifiers");
	Collection<RequestScopeFunction> readWhereGrantedIsTrueByScopeFunctionsIdentifiers(Collection<String> scopeFunctionsIdentifiers);
	
	String QUERY_IDENTIFIER_COUNT_WHERE_GRANTED_IS_TRUE_BY_SCOPE_FUNCTIONS_IDENTIFIERS = QueryIdentifierBuilder.getInstance().build(RequestScopeFunction.class, "countWhereGrantedIsTrueByScopeFunctionsIdentifiers");
	Long countWhereGrantedIsTrueByScopeFunctionsIdentifiers(Collection<String> scopeFunctionsIdentifiers);
	
	Integer updateGrantedToFalseWhereTrueByScopeFunctionsIdentifiers(Collection<String> scopeFunctionsIdentifiers,String auditActor,String auditFunctionality,String auditAction,LocalDateTime auditDate,EntityManager entityManager);
	Integer updateGrantedToFalseWhereTrueByScopeFunctionsIdentifiers(EntityManager entityManager,String auditActor,String auditFunctionality,String auditAction,LocalDateTime auditDate,String...scopeFunctionsIdentifiers);
	
	String QUERY_IDENTIFIER_READ_DYNAMIC = QueryIdentifierBuilder.getInstance().build(RequestScopeFunction.class, QueryName.READ_DYNAMIC);	
	String QUERY_IDENTIFIER_READ_DYNAMIC_ONE = QueryIdentifierBuilder.getInstance().build(RequestScopeFunction.class, QueryName.READ_DYNAMIC_ONE);
	String QUERY_IDENTIFIER_COUNT_DYNAMIC = QueryIdentifierBuilder.getInstance().build(RequestScopeFunction.class, QueryName.COUNT_DYNAMIC);
	
	RequestScopeFunction readOne(QueryExecutorArguments arguments);
	Collection<RequestScopeFunction> readMany(QueryExecutorArguments arguments);
	Long count(QueryExecutorArguments arguments);
	
	public static abstract class AbstractImpl extends Querier.AbstractImpl implements RequestScopeFunctionQuerier,Serializable {
		
		@Override
		public RequestScopeFunction readOne(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_DYNAMIC_ONE.equals(arguments.getQuery().getIdentifier()))
				return DynamicOneExecutor.getInstance().read(RequestScopeFunction.class,arguments.setQuery(null));
			throw new RuntimeException(arguments.getQuery().getIdentifier()+" cannot be processed");
		}
		
		@Override
		public Collection<RequestScopeFunction> readMany(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_DYNAMIC.equals(arguments.getQuery().getIdentifier()))
				return DynamicManyExecutor.getInstance().read(RequestScopeFunction.class,arguments.setQuery(null));
			throw new RuntimeException(arguments.getQuery().getIdentifier()+" cannot be processed");
		}
		
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_COUNT_DYNAMIC.equals(arguments.getQuery().getIdentifier()))
				return DynamicManyExecutor.getInstance().count(RequestScopeFunction.class,arguments.setQuery(null));
			throw new RuntimeException(arguments.getQuery().getIdentifier()+" cannot be processed");
		}
		
		@Override
		public Collection<RequestScopeFunction> readByRequestsIdentifiers(Collection<String> requestsIdentifiers) {
			if(CollectionHelper.isEmpty(requestsIdentifiers))
				return null;
			Collection<RequestScopeFunction> requestFunctions = QueryExecutor.getInstance().executeReadMany(RequestScopeFunction.class
					, QUERY_IDENTIFIER_READ_BY_REQUESTS_IDENTIFIERS,PARAMETER_NAME_REQUESTS_IDENTIFIERS,requestsIdentifiers);
			return requestFunctions;
		}
		
		@Override
		public Collection<RequestScopeFunction> readUsingScalarModeByRequestsIdentifiers(Collection<String> requestsIdentifiers) {
			if(CollectionHelper.isEmpty(requestsIdentifiers))
				return null;
			Collection<RequestScopeFunction> requestFunctions = QueryExecutor.getInstance().executeReadMany(RequestScopeFunction.class
					, QUERY_IDENTIFIER_READ_USING_SCALAR_MODE_BY_REQUESTS_IDENTIFIERS,PARAMETER_NAME_REQUESTS_IDENTIFIERS,requestsIdentifiers);
			return requestFunctions;
		}
		
		@Override
		public Collection<RequestScopeFunction> readWhereGrantedIsTrueByRequestsIdentifiers(Collection<String> requestsIdentifiers) {
			if(CollectionHelper.isEmpty(requestsIdentifiers))
				return null;
			return QueryExecutor.getInstance().executeReadMany(RequestScopeFunction.class, QUERY_IDENTIFIER_READ_WHERE_GRANTED_IS_TRUE_BY_REQUESTS_IDENTIFIERS
					,PARAMETER_NAME_REQUESTS_IDENTIFIERS,requestsIdentifiers);
		}
		
		@Override
		public Collection<Object[]> countWhereGrantedIsTrueGroupByRequestIdentifierByRequestsIdentifiers(Collection<String> requestsIdentifiers) {
			if(CollectionHelper.isEmpty(requestsIdentifiers))
				return null;
			javax.persistence.TypedQuery<Object[]> query = EntityManagerGetter.getInstance().get()
					.createNamedQuery(QUERY_IDENTIFIER_COUNT_WHERE_GRANTED_IS_TRUE_GROUP_BY_REQUEST_IDENTIFIER_BY_REQUESTS_IDENTIFIERS,Object[].class);
			query.setParameter(PARAMETER_NAME_REQUESTS_IDENTIFIERS, requestsIdentifiers);
			return query.getResultList();
		}
		
		@Override
		public Collection<Object[]> countWhereGrantedIsTrueGroupByScopeFunctionIdentifierByScopeFunctionsIdentifiers(Collection<String> scopeFunctionsIdentifiers) {
			if(CollectionHelper.isEmpty(scopeFunctionsIdentifiers))
				return null;
			javax.persistence.TypedQuery<Object[]> query = EntityManagerGetter.getInstance().get()
					.createNamedQuery(QUERY_IDENTIFIER_COUNT_WHERE_GRANTED_IS_TRUE_GROUP_BY_SCOPE_FUNCTION_IDENTIFIER_BY_SCOPE_FUNCTIONS_IDENTIFIERS,Object[].class);
			query.setParameter(PARAMETER_NAME_SCOPE_FUNCTIONS_IDENTIFIERS, scopeFunctionsIdentifiers);
			return query.getResultList();
		}
		
		@Override
		public Collection<Object[]> countWhereRequestedIsTrueGroupByScopeFunctionIdentifierByScopeFunctionsIdentifiers(Collection<String> scopeFunctionsIdentifiers) {
			if(CollectionHelper.isEmpty(scopeFunctionsIdentifiers))
				return null;
			javax.persistence.TypedQuery<Object[]> query = EntityManagerGetter.getInstance().get()
					.createNamedQuery(QUERY_IDENTIFIER_COUNT_WHERE_REQUESTED_IS_TRUE_GROUP_BY_SCOPE_FUNCTION_IDENTIFIER_BY_SCOPE_FUNCTIONS_IDENTIFIERS,Object[].class);
			query.setParameter(PARAMETER_NAME_SCOPE_FUNCTIONS_IDENTIFIERS, scopeFunctionsIdentifiers);
			return query.getResultList();
		}
		
		@Override
		public Collection<RequestScopeFunction> readByScopeFunctionsIdentifiers(Collection<String> scopeFunctionsIdentifiers) {
			if(CollectionHelper.isEmpty(scopeFunctionsIdentifiers))
				return null;
			return QueryExecutor.getInstance().executeReadMany(RequestScopeFunction.class, QUERY_IDENTIFIER_READ_BY_SCOPE_FUNCTIONS_IDENTIFIERS
					,PARAMETER_NAME_SCOPE_FUNCTIONS_IDENTIFIERS,scopeFunctionsIdentifiers);
		}
		
		@Override
		public RequestScopeFunction readByRequestIdentifierByScopeFunctionIdentifier(String requestIdentifier,String scopeFunctionIdentifier) {
			if(StringHelper.isBlank(requestIdentifier) || StringHelper.isBlank(scopeFunctionIdentifier))
				return null;
			return QueryExecutor.getInstance().executeReadOne(RequestScopeFunction.class, new QueryExecutorArguments()
					.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_BY_REQUESTS_IDENTIFIER_BY_SCOPE_FUNCTIONS_IDENTIFIER)
					.addFilterFieldsValues(PARAMETER_NAME_REQUEST_IDENTIFIER,requestIdentifier,PARAMETER_NAME_SCOPE_FUNCTION_IDENTIFIER,scopeFunctionIdentifier));
		}
		
		@Override
		public Collection<RequestScopeFunction> readWhereGrantedIsTrueByScopeFunctionsIdentifiers(Collection<String> scopeFunctionsIdentifiers) {
			if(CollectionHelper.isEmpty(scopeFunctionsIdentifiers))
				return null;
			return QueryExecutor.getInstance().executeReadMany(RequestScopeFunction.class, QUERY_IDENTIFIER_READ_WHERE_GRANTED_IS_TRUE_BY_SCOPE_FUNCTIONS_IDENTIFIERS
					,PARAMETER_NAME_SCOPE_FUNCTIONS_IDENTIFIERS,scopeFunctionsIdentifiers);
		}
		
		@Override
		public Long countWhereGrantedIsTrueByScopeFunctionsIdentifiers(Collection<String> scopeFunctionsIdentifiers) {
			if(CollectionHelper.isEmpty(scopeFunctionsIdentifiers))
				return null;
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT_WHERE_GRANTED_IS_TRUE_BY_SCOPE_FUNCTIONS_IDENTIFIERS
					,PARAMETER_NAME_SCOPE_FUNCTIONS_IDENTIFIERS,scopeFunctionsIdentifiers);
		}
	}
	
	/**/
	
	/**/
	
	static RequestScopeFunctionQuerier getInstance() {
		return Helper.getInstance(RequestScopeFunctionQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		QueryManager.getInstance().register(
			Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_REQUESTS_IDENTIFIERS
					,Query.FIELD_TUPLE_CLASS,RequestScopeFunction.class,Query.FIELD_RESULT_CLASS,RequestScopeFunction.class
					,Query.FIELD_VALUE,jpql("SELECT t","FROM RequestScopeFunction t","WHERE t.request.identifier IN :"+PARAMETER_NAME_REQUESTS_IDENTIFIERS,"ORDER BY t.scopeFunction.code ASC"))
			
			,Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_USING_SCALAR_MODE_BY_REQUESTS_IDENTIFIERS
					,Query.FIELD_TUPLE_CLASS,RequestScopeFunction.class,Query.FIELD_RESULT_CLASS,RequestScopeFunction.class
					,Query.FIELD_VALUE,jpql("SELECT t.identifier,t.request.identifier,t.scopeFunction.identifier,t.scopeFunction.code,t.scopeFunction.name"
							+ ",t.scopeFunction.function.code,t.requested,t.granted","FROM RequestScopeFunction t"
							,"WHERE t.request.identifier IN :"+PARAMETER_NAME_REQUESTS_IDENTIFIERS,"ORDER BY t.identifier ASC"))
					.setTupleFieldsNamesIndexesFromFieldsNames(RequestScopeFunction.FIELD_IDENTIFIER,RequestScopeFunction.FIELD_REQUEST_IDENTIFIER
							,RequestScopeFunction.FIELD_SCOPE_FUNCTION_IDENTIFIER,RequestScopeFunction.FIELD_SCOPE_FUNCTION_CODE,RequestScopeFunction.FIELD_SCOPE_FUNCTION_NAME
							,RequestScopeFunction.FIELD_FUNCTION_CODE,RequestScopeFunction.FIELD_REQUESTED,RequestScopeFunction.FIELD_GRANTED)
			
			,Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_WHERE_GRANTED_IS_TRUE_BY_REQUESTS_IDENTIFIERS
					,Query.FIELD_TUPLE_CLASS,RequestScopeFunction.class,Query.FIELD_RESULT_CLASS,RequestScopeFunction.class
					,Query.FIELD_VALUE,jpql("SELECT t.identifier,t.request.identifier,t.scopeFunction.identifier,t.requested,t.granted"
							,"FROM RequestScopeFunction t","WHERE t.granted = true AND t.request.identifier IN :"+PARAMETER_NAME_REQUESTS_IDENTIFIERS
							,"ORDER BY t.identifier ASC")).setTupleFieldsNamesIndexesFromFieldsNames(RequestScopeFunction.FIELD_IDENTIFIER
									,RequestScopeFunction.FIELD_REQUEST_IDENTIFIER,RequestScopeFunction.FIELD_SCOPE_FUNCTION_IDENTIFIER,RequestScopeFunction.FIELD_REQUESTED
									,RequestScopeFunction.FIELD_GRANTED)		
			
			,Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_WHERE_GRANTED_IS_TRUE_GROUP_BY_REQUEST_IDENTIFIER_BY_REQUESTS_IDENTIFIERS
					,Query.FIELD_TUPLE_CLASS,RequestScopeFunction.class,Query.FIELD_RESULT_CLASS,Object[].class
					,Query.FIELD_VALUE,jpql("SELECT t.request.identifier,COUNT(t.identifier)"
							,"FROM RequestScopeFunction t","WHERE t.granted = true AND t.request.identifier IN :"+PARAMETER_NAME_REQUESTS_IDENTIFIERS
							,"GROUP BY t.request.identifier"))		
			
			,Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_WHERE_REQUESTED_IS_TRUE_GROUP_BY_SCOPE_FUNCTION_IDENTIFIER_BY_SCOPE_FUNCTIONS_IDENTIFIERS
					,Query.FIELD_TUPLE_CLASS,RequestScopeFunction.class,Query.FIELD_RESULT_CLASS,Object[].class
					,Query.FIELD_VALUE,jpql("SELECT t.scopeFunction.identifier,COUNT(t.identifier)"
							,"FROM RequestScopeFunction t","WHERE t.requested = true AND t.scopeFunction.identifier IN :"+PARAMETER_NAME_SCOPE_FUNCTIONS_IDENTIFIERS
							,"GROUP BY t.scopeFunction.identifier"))
			
			,Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_WHERE_GRANTED_IS_TRUE_GROUP_BY_SCOPE_FUNCTION_IDENTIFIER_BY_SCOPE_FUNCTIONS_IDENTIFIERS
					,Query.FIELD_TUPLE_CLASS,RequestScopeFunction.class,Query.FIELD_RESULT_CLASS,Object[].class
					,Query.FIELD_VALUE,jpql("SELECT t.scopeFunction.identifier,COUNT(t.identifier)"
							,"FROM RequestScopeFunction t","WHERE t.granted = true AND t.scopeFunction.identifier IN :"+PARAMETER_NAME_SCOPE_FUNCTIONS_IDENTIFIERS
							,"GROUP BY t.scopeFunction.identifier"))
			
			,Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_SCOPE_FUNCTIONS_IDENTIFIERS
					,Query.FIELD_TUPLE_CLASS,RequestScopeFunction.class,Query.FIELD_RESULT_CLASS,RequestScopeFunction.class
					,Query.FIELD_VALUE,jpql("SELECT t","FROM RequestScopeFunction t","WHERE t.scopeFunction.identifier IN :"+PARAMETER_NAME_SCOPE_FUNCTIONS_IDENTIFIERS,"ORDER BY t.identifier ASC"))
			
			,Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_WHERE_GRANTED_IS_TRUE_BY_SCOPE_FUNCTIONS_IDENTIFIERS
					,Query.FIELD_TUPLE_CLASS,RequestScopeFunction.class,Query.FIELD_RESULT_CLASS,RequestScopeFunction.class
					,Query.FIELD_VALUE,jpql("SELECT t","FROM RequestScopeFunction t","WHERE t.granted = true AND t.scopeFunction.identifier IN :"+PARAMETER_NAME_SCOPE_FUNCTIONS_IDENTIFIERS,"ORDER BY t.identifier ASC"))
			
			,Query.buildCount(QUERY_IDENTIFIER_COUNT_WHERE_GRANTED_IS_TRUE_BY_SCOPE_FUNCTIONS_IDENTIFIERS
					,jpql("SELECT COUNT(t)","FROM RequestScopeFunction t","WHERE t.granted = true AND t.scopeFunction.identifier IN :"+PARAMETER_NAME_SCOPE_FUNCTIONS_IDENTIFIERS))
			
			,Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_REQUESTS_IDENTIFIER_BY_SCOPE_FUNCTIONS_IDENTIFIER
					,Query.FIELD_TUPLE_CLASS,RequestScopeFunction.class,Query.FIELD_RESULT_CLASS,RequestScopeFunction.class
					,Query.FIELD_VALUE,jpql("SELECT t","FROM RequestScopeFunction t","WHERE t.request.identifier = :"+PARAMETER_NAME_REQUEST_IDENTIFIER
							+" AND t.scopeFunction.identifier = :"+PARAMETER_NAME_SCOPE_FUNCTION_IDENTIFIER))
			
		);
	}
}