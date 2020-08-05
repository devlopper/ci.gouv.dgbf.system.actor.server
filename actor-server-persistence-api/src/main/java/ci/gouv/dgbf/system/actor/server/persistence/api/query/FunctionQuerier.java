package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.persistence.query.EntityCounter;
import org.cyk.utility.__kernel__.persistence.query.EntityReader;
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

@Queries(value = {
		@org.cyk.utility.__kernel__.persistence.query.annotation.Query(tupleClass = Function.class,name = FunctionQuerier.QUERY_NAME_READ,value = "SELECT t FROM Function t ORDER BY t.code ASC")
})
public interface FunctionQuerier extends Querier.CodableAndNamable<Function> {

	String PARAMETER_NAME_ACCOUNT_REQUEST_IDENTIFIER = "accountRequestIdentifier";
	
	/* read order by code ascending */
	String QUERY_NAME_READ = "readOrderByCodeAscending";
	String QUERY_IDENTIFIER_READ = QueryIdentifierBuilder.getInstance().build(Function.class, QUERY_NAME_READ);
	//Collection<FunctionType> readOrderByCodeAscending();
	
	String PARAMETER_NAME_TYPES_CODES = "typesCodes";
	
	/* read by types codes order by code ascending */
	String QUERY_NAME_READ_BY_TYPES_CODES = "readByTypesCodes";
	String QUERY_IDENTIFIER_READ_BY_TYPES_CODES = QueryIdentifierBuilder.getInstance().build(Function.class, QUERY_NAME_READ_BY_TYPES_CODES);
	String QUERY_VALUE_READ_BY_TYPES_CODES = Language.of(Language.Select.of("t")
			,Language.From.of("Function t")			
			,Language.Where.of("t.type.code IN :"+PARAMETER_NAME_TYPES_CODES)
			,Language.Order.of("t.code ASC"))
			;
	Collection<Function> readByTypesCodes(Collection<String> typesCodes);
	
	/* count by types codes */
	String QUERY_NAME_COUNT_BY_TYPES_CODES = "countByTypesCodes";
	String QUERY_IDENTIFIER_COUNT_BY_TYPES_CODES = QueryIdentifierBuilder.getInstance().build(Function.class, QUERY_NAME_COUNT_BY_TYPES_CODES);
	String QUERY_VALUE_COUNT_BY_TYPES_CODES = Language.of(Language.Select.of("COUNT(t.identifier)")
			,Language.From.of("Function t")			
			,Language.Where.of("t.type.code IN :"+PARAMETER_NAME_TYPES_CODES))
			;
	Long countByTypesCodes(Collection<String> typesCodes);
	
	/* read with profiles by types codes order by code ascending */
	String QUERY_NAME_READ_WITH_PROFILES_BY_TYPES_CODES = "readWithProfilesByTypesCodes";
	String QUERY_IDENTIFIER_READ_WITH_PROFILES_BY_TYPES_CODES = QueryIdentifierBuilder.getInstance().build(Function.class, QUERY_NAME_READ_WITH_PROFILES_BY_TYPES_CODES);
	
	/* count with profiles */
	String QUERY_NAME_COUNT_WITH_PROFILES_BY_TYPES_CODES = "countWithProfilesByTypesCodes";
	String QUERY_IDENTIFIER_COUNT_WITH_PROFILES_BY_TYPES_CODES = QueryIdentifierBuilder.getInstance().build(Function.class, QUERY_NAME_COUNT_WITH_PROFILES_BY_TYPES_CODES);
	
	/* read with profiles order by code ascending */
	String QUERY_NAME_READ_WITH_PROFILES = "readWithProfiles";
	String QUERY_IDENTIFIER_READ_WITH_PROFILES = QueryIdentifierBuilder.getInstance().build(Function.class, QUERY_NAME_READ_WITH_PROFILES);
	
	/* count with profiles */
	String QUERY_NAME_COUNT_WITH_PROFILES = "countWithProfiles";
	String QUERY_IDENTIFIER_COUNT_WITH_PROFILES = QueryIdentifierBuilder.getInstance().build(Function.class, QUERY_NAME_COUNT_WITH_PROFILES);
	
	/*read by account request identifier*/
	String QUERY_IDENTIFIER_READ_BY_ACCOUNT_REQUEST_IDENTIFIER = Querier.buildIdentifier(Function.class,"readByAccountRequestIdentifier");
	Collection<Function> readByAccountRequestIdentifier(String accountRequestIdentifier);
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<Function> implements FunctionQuerier,Serializable {
		
		@Override
		public Collection<Function> readByTypesCodes(Collection<String> typesCodes) {
			return EntityReader.getInstance().readMany(Function.class, QUERY_IDENTIFIER_READ_BY_TYPES_CODES, PARAMETER_NAME_TYPES_CODES,typesCodes);
		}
		
		@Override
		public Long countByTypesCodes(Collection<String> typesCodes) {
			return EntityCounter.getInstance().count(Function.class, QUERY_IDENTIFIER_COUNT_BY_TYPES_CODES,PARAMETER_NAME_TYPES_CODES,typesCodes);
		}
		
		@Override
		public Collection<Function> readByAccountRequestIdentifier(String accountRequestIdentifier) {
			return QueryExecutor.getInstance().executeReadMany(Function.class,QUERY_IDENTIFIER_READ_BY_ACCOUNT_REQUEST_IDENTIFIER, PARAMETER_NAME_ACCOUNT_REQUEST_IDENTIFIER, accountRequestIdentifier);
		}
		
		@Override
		public Collection<Function> readMany(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_BY_ACCOUNT_REQUEST_IDENTIFIER.equals(arguments.getQuery().getIdentifier()))
				return readByAccountRequestIdentifier((String) arguments.getFilterFieldValue(PARAMETER_NAME_ACCOUNT_REQUEST_IDENTIFIER));
			return super.readMany(arguments);
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
				,Query.FIELD_VALUE,QUERY_VALUE_READ_BY_TYPES_CODES
				)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_BY_TYPES_CODES
				,Query.FIELD_TUPLE_CLASS,Function.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_BY_TYPES_CODES
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