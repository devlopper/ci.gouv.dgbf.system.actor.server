package ci.gouv.dgbf.system.actor.server.persistence.api.query;

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

@Queries(value = {
	@org.cyk.utility.__kernel__.persistence.query.annotation.Query(tupleClass = Function.class,name = FunctionQuerier.QUERY_NAME_READ,value = "SELECT t FROM Function t ORDER BY t.code ASC")
	//,@org.cyk.utility.__kernel__.persistence.query.annotation.Query(tupleClass = Function.class,name = FunctionQuerier.QUERY_NAME_COUNT,value = "SELECT COUNT(t.identifier) FROM Function t")
})
public interface FunctionQuerier extends Querier.CodableAndNamable<Function> {

	String PARAMETER_NAME_ACCOUNT_REQUEST_IDENTIFIER = "accountRequestIdentifier";
	String PARAMETER_NAME_TYPES_CODES = "typesCodes";
	
	/* read order by code ascending */
	String QUERY_NAME_READ = "readOrderByCodeAscending";
	String QUERY_IDENTIFIER_READ = QueryIdentifierBuilder.getInstance().build(Function.class, QUERY_NAME_READ);
	Collection<Function> read();
	String QUERY_IDENTIFIER_COUNT = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ);
	Long count();
	
	/* read by types codes order by code ascending */
	String QUERY_IDENTIFIER_READ_BY_TYPES_CODES = QueryIdentifierBuilder.getInstance().build(Function.class, "readByTypesCodes");
	Collection<Function> readByTypesCodes(Collection<String> typesCodes);
	String QUERY_IDENTIFIER_COUNT_BY_TYPES_CODES = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_BY_TYPES_CODES);
	Long countByTypesCodes(Collection<String> typesCodes);
	
	/* read with profiles order by code ascending */
	String QUERY_IDENTIFIER_READ_WITH_PROFILES = QueryIdentifierBuilder.getInstance().build(Function.class, "readWithProfiles");
	Collection<Function> readWithProfiles();
	
	/* read with profiles by types codes order by code ascending */
	String QUERY_IDENTIFIER_READ_WITH_PROFILES_BY_TYPES_CODES = QueryIdentifierBuilder.getInstance().build(Function.class, "readWithProfilesByTypesCodes");
	Collection<Function> readWithProfilesByTypesCodes(Collection<String> typesCodes);
	
	/*read by account request identifier*/
	String QUERY_IDENTIFIER_READ_BY_ACCOUNT_REQUEST_IDENTIFIER = Querier.buildIdentifier(Function.class,"readByAccountRequestIdentifier");
	Collection<Function> readByAccountRequestIdentifier(String accountRequestIdentifier);
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<Function> implements FunctionQuerier,Serializable {
		
		@Override
		public Collection<Function> read() {
			return QueryExecutor.getInstance().executeReadMany(Function.class,QUERY_IDENTIFIER_READ);
		}
		
		@Override
		public Long count() {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT);
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