package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import static org.cyk.utility.__kernel__.persistence.query.Language.jpql;
import static org.cyk.utility.__kernel__.persistence.query.Language.From.from;
import static org.cyk.utility.__kernel__.persistence.query.Language.Order.asc;
import static org.cyk.utility.__kernel__.persistence.query.Language.Order.order;
import static org.cyk.utility.__kernel__.persistence.query.Language.Select.select;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.exists;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.where;

import java.io.Serializable;
import java.util.Collection;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.persistence.query.Language;
import org.cyk.utility.__kernel__.persistence.query.Language.From;
import org.cyk.utility.__kernel__.persistence.query.Language.Select;
import org.cyk.utility.__kernel__.persistence.query.Language.Where;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.persistence.query.annotation.Queries;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeTypeFunction;

@Queries(value = {
	@org.cyk.utility.__kernel__.persistence.query.annotation.Query(tupleClass = Function.class,name = FunctionQuerier.QUERY_NAME_READ,value = "SELECT t FROM Function t ORDER BY t.code ASC")
	//,@org.cyk.utility.__kernel__.persistence.query.annotation.Query(tupleClass = Function.class,name = FunctionQuerier.QUERY_NAME_COUNT,value = "SELECT COUNT(t.identifier) FROM Function t")
})
public interface FunctionQuerier extends Querier.CodableAndNamable<Function> {

	String PARAMETER_NAME_ACCOUNT_REQUEST_IDENTIFIER = "accountRequestIdentifier";
	String PARAMETER_NAME_TYPES_CODES = "typesCodes";
	String PARAMETER_NAME_SCOPE_TYPE_CODES = "scopeTypeCodes";
	
	/* read order by code ascending */
	String QUERY_NAME_READ = "readOrderByCodeAscending";
	String QUERY_IDENTIFIER_READ = QueryIdentifierBuilder.getInstance().build(Function.class, QUERY_NAME_READ);
	Collection<Function> read();
	String QUERY_IDENTIFIER_COUNT = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ);
	Long count();
	
	/* read by types codes order by code ascending */
	String QUERY_IDENTIFIER_READ_BY_TYPES_CODES = QueryIdentifierBuilder.getInstance().build(Function.class, "readByTypesCodes");
	Collection<Function> readByTypesCodes(Collection<String> typesCodes);
	/* count by types codes */
	String QUERY_IDENTIFIER_COUNT_BY_TYPES_CODES = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_BY_TYPES_CODES);
	Long countByTypesCodes(Collection<String> typesCodes);
	
	/* read by scope type codes order by code ascending */
	String QUERY_IDENTIFIER_READ_BY_SCOPE_TYPE_CODES = QueryIdentifierBuilder.getInstance().build(Function.class, "readByScopeTypeCodes");
	Collection<Function> readByScopeTypeCodes(Collection<String> scopeTypeCodes);
	/* count by scope type codes */
	String QUERY_IDENTIFIER_COUNT_BY_SCOPE_TYPE_CODES = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_BY_SCOPE_TYPE_CODES);
	Long countByScopeTypeCodes(Collection<String> scopeTypeCodes);
	
	/* read with profiles order by code ascending */
	String QUERY_IDENTIFIER_READ_WITH_PROFILES = QueryIdentifierBuilder.getInstance().build(Function.class, "readWithProfiles");
	Collection<Function> readWithProfiles();
	
	/* read with profiles by types codes order by code ascending */
	String QUERY_IDENTIFIER_READ_WITH_PROFILES_BY_TYPES_CODES = QueryIdentifierBuilder.getInstance().build(Function.class, "readWithProfilesByTypesCodes");
	Collection<Function> readWithProfilesByTypesCodes(Collection<String> typesCodes);
	
	/*read by account request identifier*/
	String QUERY_IDENTIFIER_READ_BY_ACCOUNT_REQUEST_IDENTIFIER = Querier.buildIdentifier(Function.class,"readByAccountRequestIdentifier");
	Collection<Function> readByAccountRequestIdentifier(String accountRequestIdentifier);
	
	/*read where associated to scope type */
	String QUERY_IDENTIFIER_READ_WHERE_ASSOCIATED_TO_SCOPE_TYPE = Querier.buildIdentifier(Function.class,"readWhereAssociatedToScopeType");
	Collection<Function> readWhereAssociatedToScopeType(QueryExecutorArguments arguments);
	
	/*read where associated to scope type with all */
	String QUERY_IDENTIFIER_READ_WHERE_ASSOCIATED_TO_SCOPE_TYPE_WITH_ALL = Querier.buildIdentifier(Function.class,"readWhereAssociatedToScopeTypeWithAll");
	Collection<Function> readWhereAssociatedToScopeTypeWithAll(QueryExecutorArguments arguments);
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<Function> implements FunctionQuerier,Serializable {
		
		@Override
		public Collection<Function> readWhereAssociatedToScopeType(QueryExecutorArguments arguments) {
			return QueryExecutor.getInstance().executeReadMany(Function.class,QUERY_IDENTIFIER_READ_WHERE_ASSOCIATED_TO_SCOPE_TYPE);
		}
		
		@Override
		public Collection<Function> readWhereAssociatedToScopeTypeWithAll(QueryExecutorArguments arguments) {
			Collection<Function> functions = readWhereAssociatedToScopeType(arguments);
			if(CollectionHelper.isEmpty(functions))
				return null;
			Collection<ScopeTypeFunction> scopeTypeFunctions = ScopeTypeFunctionQuerier.getInstance().read();
			if(CollectionHelper.isNotEmpty(scopeTypeFunctions))
				functions.forEach(function -> {
					function.setScopeTypes(scopeTypeFunctions.stream().filter(scopeTypeFunction -> scopeTypeFunction.getFunction().equals(function))
							.map(scopeTypeFunction -> scopeTypeFunction.getScopeType()).collect(Collectors.toList()));
				});	
			return functions;
		}
		
		@Override
		public Collection<Function> read() {
			return QueryExecutor.getInstance().executeReadMany(Function.class,QUERY_IDENTIFIER_READ);
		}
		
		@Override
		public Long count() {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT);
		}
		
		@Override
		public Collection<Function> readByScopeTypeCodes(Collection<String> scopeTypesCodes) {
			return QueryExecutor.getInstance().executeReadMany(Function.class, QUERY_IDENTIFIER_READ_BY_SCOPE_TYPE_CODES, PARAMETER_NAME_SCOPE_TYPE_CODES,scopeTypesCodes);
		}
		
		@Override
		public Long countByScopeTypeCodes(Collection<String> scopeTypesCodes) {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT_BY_SCOPE_TYPE_CODES,PARAMETER_NAME_SCOPE_TYPE_CODES,scopeTypesCodes);
		}
		
		@Override
		public Collection<Function> readByTypesCodes(Collection<String> typesCodes) {
			return QueryExecutor.getInstance().executeReadMany(Function.class, QUERY_IDENTIFIER_READ_BY_TYPES_CODES, PARAMETER_NAME_TYPES_CODES,typesCodes);
		}
		
		@Override
		public Long countByTypesCodes(Collection<String> typesCodes) {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT_BY_TYPES_CODES,PARAMETER_NAME_TYPES_CODES,typesCodes);
		}
		
		@Override
		public Collection<Function> readByAccountRequestIdentifier(String accountRequestIdentifier) {
			return QueryExecutor.getInstance().executeReadMany(Function.class,QUERY_IDENTIFIER_READ_BY_ACCOUNT_REQUEST_IDENTIFIER, PARAMETER_NAME_ACCOUNT_REQUEST_IDENTIFIER, accountRequestIdentifier);
		}
		
		@Override
		public Collection<Function> readWithProfiles() {
			Collection<Function> functions = read();
			if(CollectionHelper.isEmpty(functions))
				return null;
			__setProfiles__(functions);
			return functions;
		}
		
		@Override
		public Collection<Function> readWithProfilesByTypesCodes(Collection<String> typesCodes) {
			Collection<Function> functions = readByTypesCodes(typesCodes);
			if(CollectionHelper.isEmpty(functions))
				return null;
			__setProfiles__(functions);
			return functions;
		}
		
		private static void __setProfiles__(Collection<Function> functions) {
			if(CollectionHelper.isEmpty(functions))
				return;
			Collection<ProfileFunction> profileFunctions = ProfileFunctionQuerier.getInstance().readByFunctionsCodes(functions.stream().map(x -> x.getCode()).collect(Collectors.toList()));
			if(CollectionHelper.isNotEmpty(profileFunctions))
				functions.forEach(function -> {
					Collection<ProfileFunction> __profileFunctions__ = profileFunctions.stream().filter(profileFunction -> profileFunction.getFunction().equals(function)).collect(Collectors.toList());
					if(CollectionHelper.isNotEmpty(__profileFunctions__))
						function.setProfilesAsStrings(__profileFunctions__.stream().map(profileFunction -> profileFunction.getProfile().getName()).collect(Collectors.toList()));
				});
		}
		
		@SuppressWarnings("unchecked")
		@Override
		public Collection<Function> readMany(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ.equals(arguments.getQuery().getIdentifier()))
				return read();
			if(QUERY_IDENTIFIER_READ_BY_TYPES_CODES.equals(arguments.getQuery().getIdentifier()))
				return readByTypesCodes((Collection<String>) arguments.getFilterFieldValue(PARAMETER_NAME_TYPES_CODES));
			if(QUERY_IDENTIFIER_READ_WITH_PROFILES.equals(arguments.getQuery().getIdentifier()))
				return readWithProfiles();
			if(QUERY_IDENTIFIER_READ_WITH_PROFILES_BY_TYPES_CODES.equals(arguments.getQuery().getIdentifier()))
				return readWithProfilesByTypesCodes((Collection<String>) arguments.getFilterFieldValue(PARAMETER_NAME_TYPES_CODES));
			if(QUERY_IDENTIFIER_READ_BY_ACCOUNT_REQUEST_IDENTIFIER.equals(arguments.getQuery().getIdentifier()))
				return readByAccountRequestIdentifier((String) arguments.getFilterFieldValue(PARAMETER_NAME_ACCOUNT_REQUEST_IDENTIFIER));
			if(QUERY_IDENTIFIER_READ_WHERE_ASSOCIATED_TO_SCOPE_TYPE.equals(arguments.getQuery().getIdentifier()))
				return readWhereAssociatedToScopeType(arguments);
			if(QUERY_IDENTIFIER_READ_WHERE_ASSOCIATED_TO_SCOPE_TYPE_WITH_ALL.equals(arguments.getQuery().getIdentifier()))
				return readWhereAssociatedToScopeTypeWithAll(arguments);
			return super.readMany(arguments);
		}
		
		@SuppressWarnings("unchecked")
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_COUNT.equals(arguments.getQuery().getIdentifier()))
				return count();
			if(QUERY_IDENTIFIER_COUNT_BY_TYPES_CODES.equals(arguments.getQuery().getIdentifier()))
				return countByTypesCodes((Collection<String>) arguments.getFilterFieldValue(PARAMETER_NAME_TYPES_CODES));			
			return super.count(arguments);
		}
		
		@Override
		protected Class<Function> getKlass() {
			return Function.class;
		}
	}
	
	/**/
	
	static FunctionQuerier getInstance() {
		return Helper.getInstance(FunctionQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		Querier.CodableAndNamable.initialize(Function.class);
		
		QueryHelper.addQueries(
				Query.buildSelect(Function.class, QUERY_IDENTIFIER_READ_BY_SCOPE_TYPE_CODES, jpql(select("t"),from("Function t")
				,where(exists("SELECT stf FROM ScopeTypeFunction stf WHERE stf.function = t AND stf.scopeType.code IN :"+PARAMETER_NAME_SCOPE_TYPE_CODES))
				,order(asc("t", "code"))))
				,Query.buildCount(QUERY_IDENTIFIER_COUNT_BY_SCOPE_TYPE_CODES, jpql(select("COUNT(t)"),from("Function t")
						,where(exists("SELECT stf FROM ScopeTypeFunction stf WHERE stf.function = t AND stf.scopeType.code IN :"+PARAMETER_NAME_SCOPE_TYPE_CODES))))
				,Query.buildSelect(Function.class, QUERY_IDENTIFIER_READ_WHERE_ASSOCIATED_TO_SCOPE_TYPE, jpql(select("t"),from("Function t")
						,where(exists("SELECT stf FROM ScopeTypeFunction stf WHERE stf.function = t"))
						,order(asc("t", "code"))))
			);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_TYPES_CODES
				,Query.FIELD_TUPLE_CLASS,Function.class,Query.FIELD_RESULT_CLASS,Function.class
				,Query.FIELD_VALUE,Language.of(Language.Select.of("t")
						,Language.From.of("Function t")			
						,Language.Where.of("t.type.code IN :"+PARAMETER_NAME_TYPES_CODES)
						,Language.Order.of("t.code ASC"))
				)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_BY_TYPES_CODES
				,Query.FIELD_TUPLE_CLASS,Function.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,Language.of(Language.Select.of("COUNT(t.identifier)")
						,Language.From.of("Function t")			
						,Language.Where.of("t.type.code IN :"+PARAMETER_NAME_TYPES_CODES))
				)
			);
		
		QueryHelper.addQueries(
				Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_ACCOUNT_REQUEST_IDENTIFIER
				,Query.FIELD_TUPLE_CLASS,Function.class,Query.FIELD_RESULT_CLASS,Function.class
				,Query.FIELD_VALUE,Language.of(Select.of("t")
						,From.of("Function t","INNER JOIN AccountRequestFunction f ON f.function = t")
						,Where.of("f.accountRequest.identifier = :"+PARAMETER_NAME_ACCOUNT_REQUEST_IDENTIFIER))
				)
			);
	}
}