package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.persistence.query.EntityCounter;
import org.cyk.utility.__kernel__.persistence.query.EntityReader;
import org.cyk.utility.__kernel__.persistence.query.Language;
import org.cyk.utility.__kernel__.persistence.query.Language.From;
import org.cyk.utility.__kernel__.persistence.query.Language.Order;
import org.cyk.utility.__kernel__.persistence.query.Language.Select;
import org.cyk.utility.__kernel__.persistence.query.Language.Where;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileFunction;

public interface ProfileFunctionQuerier extends Querier {

	String PARAMETER_NAME_PROFILES_TYPES_CODES = "profilesTypesCodes";
	String PARAMETER_NAME_PROFILES_CODES = "profilesCodes";
	String PARAMETER_NAME_PROFILE_CODE = "profileCode";
	String PARAMETER_NAME_FUNCTIONS_CODES = "functionsCodes";
	String PARAMETER_NAME_FUNCTION_CODE = "functionCode";
	
	/* read by profile code by function code */
	String QUERY_IDENTIFIER_READ_BY_PROFILE_CODE_BY_FUNCTION_CODE = QueryIdentifierBuilder.getInstance().build(ProfileFunction.class, "readByProfileCodeByFunctionCode");
	String QUERY_VALUE_READ_BY_PROFILE_CODE_BY_FUNCTION_CODE = Language.of(Select.of("t"),From.of("ProfileFunction t")			
			,Where.of(Where.and("t.profile.code = :"+PARAMETER_NAME_PROFILE_CODE,"t.function.code = :"+PARAMETER_NAME_FUNCTION_CODE)));
	ProfileFunction readByProfileCodeByFunctionCode(String profileCode,String functionCode);
	
	/* read by profiles codes order by function code ascending */
	String QUERY_NAME_READ_BY_PROFILES_CODES = "readByProfilesCodes";
	String QUERY_IDENTIFIER_READ_BY_PROFILES_CODES = QueryIdentifierBuilder.getInstance().build(ProfileFunction.class, QUERY_NAME_READ_BY_PROFILES_CODES);
	String QUERY_VALUE_READ_BY_PROFILES_CODES = Language.of(Language.Select.of("t")
			,Language.From.of("ProfileFunction t")			
			,Language.Where.of("t.profile.code IN :"+PARAMETER_NAME_PROFILES_CODES)			
			,Language.Order.of("t.profile.code ASC,t.function.code ASC"))
			;
	Collection<ProfileFunction> readByProfilesCodes(Collection<String> profilesCodes);
	
	/* count by profiles codes */
	String QUERY_NAME_COUNT_BY_PROFILES_CODES = "countByProfilesCodes";
	String QUERY_IDENTIFIER_COUNT_BY_PROFILES_CODES = QueryIdentifierBuilder.getInstance().build(ProfileFunction.class, QUERY_NAME_COUNT_BY_PROFILES_CODES);
	String QUERY_VALUE_COUNT_BY_PROFILES_CODES = Language.of(Language.Select.of("COUNT(t)")
			,Language.From.of("ProfileFunction t")			
			,Language.Where.of("t.profile.code IN :"+PARAMETER_NAME_PROFILES_CODES)			
			);
	Long countByProfilesCodes(Collection<String> profilesCodes);
	
	/* read by functions codes order by profile code ascending */
	String QUERY_NAME_READ_BY_FUNCTIONS_CODES = "readByFunctionsCodes";
	String QUERY_IDENTIFIER_READ_BY_FUNCTIONS_CODES = QueryIdentifierBuilder.getInstance().build(ProfileFunction.class, QUERY_NAME_READ_BY_FUNCTIONS_CODES);
	String QUERY_VALUE_READ_BY_FUNCTIONS_CODES = Language.of(Language.Select.of("t")
			,Language.From.of("ProfileFunction t")			
			,Language.Where.of("t.function.code IN :"+PARAMETER_NAME_FUNCTIONS_CODES)			
			,Language.Order.of("t.profile.code ASC,t.function.code ASC"))
			;
	Collection<ProfileFunction> readByFunctionsCodes(Collection<String> functionsCodes);
	
	/* count by functions codes */
	String QUERY_NAME_COUNT_BY_FUNCTIONS_CODES = "countByFunctionsCodes";
	String QUERY_IDENTIFIER_COUNT_BY_FUNCTIONS_CODES = QueryIdentifierBuilder.getInstance().build(ProfileFunction.class, QUERY_NAME_COUNT_BY_FUNCTIONS_CODES);
	String QUERY_VALUE_COUNT_BY_FUNCTIONS_CODES = Language.of(Language.Select.of("COUNT(t)")
			,Language.From.of("ProfileFunction t")			
			,Language.Where.of("t.function.code IN :"+PARAMETER_NAME_FUNCTIONS_CODES)			
			);
	Long countByFunctionsCodes(Collection<String> functionsCodes);
	
	/* read by profiles types codes by functions codes order by profile code ascending */
	String QUERY_IDENTIFIER_READ_BY_PROFILES_TYPES_CODES_BY_FUNCTIONS_CODES = QueryIdentifierBuilder.getInstance().build(ProfileFunction.class, "readByProfilesTypesCodesByFunctionsCodes");
	Collection<ProfileFunction> readByProfilesTypesCodesByFunctionsCodes(Collection<String> profilesTypesCodes,Collection<String> functionsCodes);
	
	/* count by profiles types codes by functions codes */
	String QUERY_IDENTIFIER_COUNT_BY_PROFILES_TYPES_CODES_BY_FUNCTIONS_CODES = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_BY_PROFILES_TYPES_CODES_BY_FUNCTIONS_CODES);
	Long countByProfilesTypesCodesByFunctionsCodes(Collection<String> profilesTypesCodes,Collection<String> functionsCodes);
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.AbstractImpl implements ProfileFunctionQuerier,Serializable {
		@Override
		public ProfileFunction readByProfileCodeByFunctionCode(String profileCode, String functionCode) {
			return QueryExecutor.getInstance().executeReadOne(ProfileFunction.class, new QueryExecutorArguments()
					.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_BY_PROFILE_CODE_BY_FUNCTION_CODE)
					.addFilterFieldsValues(PARAMETER_NAME_PROFILE_CODE,profileCode,PARAMETER_NAME_FUNCTION_CODE,functionCode));
		}
		
		@Override
		public Collection<ProfileFunction> readByProfilesCodes(Collection<String> profilesCodes) {
			return EntityReader.getInstance().readMany(ProfileFunction.class, QUERY_IDENTIFIER_READ_BY_PROFILES_CODES, PARAMETER_NAME_PROFILES_CODES,profilesCodes);
		}
		
		@Override
		public Long countByProfilesCodes(Collection<String> profilesCodes) {
			return EntityCounter.getInstance().count(ProfileFunction.class, new QueryExecutorArguments().setQueryFromIdentifier(QUERY_IDENTIFIER_COUNT_BY_PROFILES_CODES)
					.addFilterField(PARAMETER_NAME_PROFILES_CODES,profilesCodes));
		}
		
		@Override
		public Collection<ProfileFunction> readByFunctionsCodes(Collection<String> functionsCodes) {
			return EntityReader.getInstance().readMany(ProfileFunction.class, QUERY_IDENTIFIER_READ_BY_FUNCTIONS_CODES, PARAMETER_NAME_FUNCTIONS_CODES,functionsCodes);
		}
		
		@Override
		public Long countByFunctionsCodes(Collection<String> functionsCodes) {
			return EntityCounter.getInstance().count(ProfileFunction.class, new QueryExecutorArguments().setQueryFromIdentifier(QUERY_IDENTIFIER_COUNT_BY_FUNCTIONS_CODES)
					.addFilterField(PARAMETER_NAME_FUNCTIONS_CODES,functionsCodes));
		}
		
		@Override
		public Collection<ProfileFunction> readByProfilesTypesCodesByFunctionsCodes(Collection<String> profilesTypesCodes, Collection<String> functionsCodes) {
			return QueryExecutor.getInstance().executeReadMany(ProfileFunction.class, QUERY_IDENTIFIER_READ_BY_PROFILES_TYPES_CODES_BY_FUNCTIONS_CODES
					, PARAMETER_NAME_PROFILES_TYPES_CODES,profilesTypesCodes,PARAMETER_NAME_FUNCTIONS_CODES,functionsCodes);
		}
		
		@Override
		public Long countByProfilesTypesCodesByFunctionsCodes(Collection<String> profilesTypesCodes,Collection<String> functionsCodes) {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT_BY_PROFILES_TYPES_CODES_BY_FUNCTIONS_CODES
					,PARAMETER_NAME_PROFILES_TYPES_CODES,profilesTypesCodes,PARAMETER_NAME_FUNCTIONS_CODES,functionsCodes);
		}
	}
	
	/**/
	
	static ProfileFunctionQuerier getInstance() {
		return Helper.getInstance(ProfileFunctionQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_PROFILE_CODE_BY_FUNCTION_CODE
				,Query.FIELD_TUPLE_CLASS,ProfileFunction.class,Query.FIELD_RESULT_CLASS,ProfileFunction.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_BY_PROFILE_CODE_BY_FUNCTION_CODE
				)
			);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_PROFILES_CODES
				,Query.FIELD_TUPLE_CLASS,ProfileFunction.class,Query.FIELD_RESULT_CLASS,ProfileFunction.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_BY_PROFILES_CODES
				)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_BY_PROFILES_CODES
				,Query.FIELD_TUPLE_CLASS,ProfileFunction.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_BY_PROFILES_CODES
				)
			);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_FUNCTIONS_CODES
				,Query.FIELD_TUPLE_CLASS,ProfileFunction.class,Query.FIELD_RESULT_CLASS,ProfileFunction.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_BY_FUNCTIONS_CODES
				)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_BY_FUNCTIONS_CODES
				,Query.FIELD_TUPLE_CLASS,ProfileFunction.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_BY_FUNCTIONS_CODES
				)
			);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_PROFILES_TYPES_CODES_BY_FUNCTIONS_CODES
				,Query.FIELD_TUPLE_CLASS,ProfileFunction.class,Query.FIELD_RESULT_CLASS,ProfileFunction.class
				,Query.FIELD_VALUE,Language.of(Language.Select.of("t")
						,From.of("ProfileFunction t")			
						,Where.of(Where.and("t.profile.type.code IN :"+PARAMETER_NAME_PROFILES_TYPES_CODES,"t.function.code IN :"+PARAMETER_NAME_FUNCTIONS_CODES))			
						,Order.of("t.profile.code ASC,t.function.code ASC"))
				)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_BY_PROFILES_TYPES_CODES_BY_FUNCTIONS_CODES
				,Query.FIELD_TUPLE_CLASS,ProfileFunction.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,Language.of(Select.of("COUNT(t)")
						,From.of("ProfileFunction t")
						,Where.of(Where.and("t.profile.type.code IN :"+PARAMETER_NAME_PROFILES_TYPES_CODES,"t.function.code IN :"+PARAMETER_NAME_FUNCTIONS_CODES))			
						)
				)
			);
	}
}