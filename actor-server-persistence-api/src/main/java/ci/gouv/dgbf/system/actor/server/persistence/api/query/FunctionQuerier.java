package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.object.AbstractObject;
import org.cyk.utility.__kernel__.persistence.query.EntityCounter;
import org.cyk.utility.__kernel__.persistence.query.EntityReader;
import org.cyk.utility.__kernel__.persistence.query.Language;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;

public interface FunctionQuerier extends Querier {

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
	
	/* read with profiles by types codes order by code ascending */
	String QUERY_NAME_COUNT_WITH_PROFILES_BY_TYPES_CODES = "countWithProfilesByTypesCodes";
	String QUERY_IDENTIFIER_COUNT_WITH_PROFILES_BY_TYPES_CODES = QueryIdentifierBuilder.getInstance().build(Function.class, QUERY_NAME_COUNT_WITH_PROFILES_BY_TYPES_CODES);
	
	/**/
	
	public static abstract class AbstractImpl extends AbstractObject implements FunctionQuerier,Serializable {
		
		@Override
		public Collection<Function> readByTypesCodes(Collection<String> typesCodes) {
			return EntityReader.getInstance().readMany(Function.class, QUERY_IDENTIFIER_READ_BY_TYPES_CODES, PARAMETER_NAME_TYPES_CODES,typesCodes);
		}
		
		@Override
		public Long countByTypesCodes(Collection<String> typesCodes) {
			return EntityCounter.getInstance().count(Function.class, QUERY_IDENTIFIER_COUNT_BY_TYPES_CODES,PARAMETER_NAME_TYPES_CODES,typesCodes);
		}
	}
	
	/**/
	
	static FunctionQuerier getInstance() {
		return Helper.getInstance(FunctionQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
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
	}
}