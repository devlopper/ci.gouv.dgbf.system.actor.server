package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import static org.cyk.utility.persistence.query.Language.jpql;
import static org.cyk.utility.persistence.query.Language.parenthesis;
import static org.cyk.utility.persistence.query.Language.From.from;
import static org.cyk.utility.persistence.query.Language.Order.asc;
import static org.cyk.utility.persistence.query.Language.Order.order;
import static org.cyk.utility.persistence.query.Language.Select.concatCodeName;
import static org.cyk.utility.persistence.query.Language.Select.select;
import static org.cyk.utility.persistence.query.Language.Where.and;
import static org.cyk.utility.persistence.query.Language.Where.like;
import static org.cyk.utility.persistence.query.Language.Where.or;
import static org.cyk.utility.persistence.query.Language.Where.where;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.value.Value;
import org.cyk.utility.persistence.query.Language.From;
import org.cyk.utility.persistence.query.Language.Order;
import org.cyk.utility.persistence.query.Language.Select;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.persistence.query.QueryIdentifierGetter;
import org.cyk.utility.persistence.query.QueryManager;
import org.cyk.utility.persistence.query.QueryName;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

public interface ScopeFunctionQuerier extends Querier.CodableAndNamable<ScopeFunction> {

	String PARAMETER_NAME_PARENTS_IDENTIFIERS = "parentsIdentifiers";
	String PARAMETER_NAME_SCOPE_TYPES_IDENTIFIERS = "scopeTypesIdentifiers";
	String PARAMETER_NAME_FUNCTIONS_IDENTIFIERS = "functionsIdentifiers";
	String PARAMETER_NAME_FUNCTION_IDENTIFIER = "functionIdentifier";
	String PARAMETER_NAME_FUNCTIONS_CODES = "functionsCodes";
	String PARAMETER_NAME_FUNCTION_CODE = "functionCode";
	String PARAMETER_NAME_SCOPE_CODE_NAME = "scopeCodeName";
	String PARAMETER_NAME_SCOPE_IDENTIFIER = "scopeIdentifier";
	//String PARAMETER_NAME_SCOPE_CODE = "scopeCode";
	//String PARAMETER_NAME_SCOPE_NAME = "scopeName";
	String PARAMETER_NAME_FUNCTION_CODE_NULLABLE = PARAMETER_NAME_FUNCTION_CODE+"Nullable";
	String PARAMETER_NAME_FUNCTION_IDENTIFIER_NULLABLE = PARAMETER_NAME_FUNCTION_IDENTIFIER+"Nullable";
	
	String QUERY_IDENTIFIER_READ_ALL_WITH_REFERENCES_ONLY = Querier.buildIdentifier(ScopeFunction.class, "readAllWithReferencesOnly");	
	Collection<ScopeFunction> readAllWithReferencesOnly(QueryExecutorArguments arguments);
	/*
	String QUERY_IDENTIFIER_READ_BY_SCOPES_IDENTIFIERS = Querier.buildIdentifier(ScopeFunction.class, "readByScopesIdentifiers");
	Collection<ScopeFunction> readByScopesIdentifiers(Collection<String> scopesIdentifiers);
	
	String QUERY_IDENTIFIER_COUNT_BY_SCOPES_IDENTIFIERS = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_BY_SCOPES_IDENTIFIERS);
	Long countByScopesIdentifiers(Collection<String> scopesIdentifiers);
	*/
	
	String QUERY_IDENTIFIER_READ_CODES_NAMES_PARENTS_IDENTIFIERS_BY_IDENTIFIERS = Querier.buildIdentifier(ScopeFunction.class, "readCodesNamesParentsIdentifiersByIdentifiers");
	Collection<ScopeFunction> readCodesNamesParentsIdentifiersByIdentifiers(Collection<String> identifiers);
	
	String QUERY_IDENTIFIER_READ_CODES_NAMES_BY_PARENTS_IDENTIFIERS = Querier.buildIdentifier(ScopeFunction.class, "readCodesNamesByParentsIdentifiers");
	Collection<ScopeFunction> readCodesNamesByParentsIdentifiers(Collection<String> identifiers);
	
	String QUERY_IDENTIFIER_READ_BY_FUNCTIONS_IDENTIFIERS = Querier.buildIdentifier(ScopeFunction.class, "readByFunctionsIdentifiers");
	Collection<ScopeFunction> readByFunctionsIdentifiers(Collection<String> functionsIdentifiers);
	
	String QUERY_IDENTIFIER_COUNT_BY_FUNCTIONS_IDENTIFIERS = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_BY_FUNCTIONS_IDENTIFIERS);
	Long countByFunctionsIdentifiers(Collection<String> functionsIdentifiers);
	
	String QUERY_IDENTIFIER_READ_BY_SCOPE_TYPES_IDENTIFIERS_BY_FUNCTIONS_IDENTIFIERS = Querier.buildIdentifier(ScopeFunction.class, "readByScopeTypesIdentifiersByFunctionsIdentifiers");
	Collection<ScopeFunction> readByScopeTypesIdentifiersByFunctionsIdentifiers(Collection<String> scopeTypesIdentifiers,Collection<String> functionsIdentifiers);
	
	String QUERY_IDENTIFIER_READ_BY_SCOPE_IDENTIFIER_BY_FUNCTION_CODE_FOR_UI = Querier.buildIdentifier(ScopeFunction.class, "readByScopeIdentifierByFunctionCodeForUI");
	Collection<ScopeFunction> readByScopeIdentifierByFunctionCodeForUI(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_READ_WHERE_CODIFICATION_DATE_IS_NULL_BY_SCOPE_TYPES_IDENTIFIERS_BY_FUNCTIONS_IDENTIFIERS = Querier.buildIdentifier(ScopeFunction.class, "readWhereCodificationDateIsNullByScopeTypesIdentifiersByFunctionsIdentifiers");
	Collection<ScopeFunction> readWhereCodificationDateIsNullByScopeTypesIdentifiersByFunctionsIdentifiers(Collection<String> scopeTypesIdentifiers,Collection<String> functionsIdentifiers);
	
	
	String QUERY_IDENTIFIER_READ_BY_FUNCTIONS_CODES = Querier.buildIdentifier(ScopeFunction.class, "readByFunctionsCodes");
	Collection<ScopeFunction> readByFunctionsCodes(QueryExecutorArguments arguments);
	Collection<ScopeFunction> readByFunctionsCodes(Collection<String> functionsCodes);
	Collection<ScopeFunction> readByFunctionsCodes(String...functionsCodes);
	
	String QUERY_IDENTIFIER_COUNT_BY_FUNCTIONS_CODES = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_BY_FUNCTIONS_CODES);
	Long countByFunctionsCodes(QueryExecutorArguments arguments);
	Long countByFunctionsCodes(Collection<String> functionsCodes);
	Long countByFunctionsCodes(String...functionsCodes);
	
	String QUERY_IDENTIFIER_READ_WHERE_CODE_OR_NAME_LIKE_BY_FUNCTION_CODE = Querier.buildIdentifier(ScopeFunction.class, "readWhereCodeOrNameLikeByFunctionCode");
	Collection<ScopeFunction> readWhereCodeOrNameLikeByFunctionCode(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_COUNT_WHERE_CODE_OR_NAME_LIKE_BY_FUNCTION_CODE = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_WHERE_CODE_OR_NAME_LIKE_BY_FUNCTION_CODE);
	Long countWhereCodeOrNameLikeByFunctionCode(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_READ_WHERE_CODE_OR_NAME_LIKE_BY_FUNCTIONS_CODES = Querier.buildIdentifier(ScopeFunction.class, "readWhereCodeOrNameLikeByFunctionsCodes");
	Collection<ScopeFunction> readWhereCodeOrNameLikeByFunctionsCodes(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_COUNT_WHERE_CODE_OR_NAME_LIKE_BY_FUNCTIONS_CODES = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_WHERE_CODE_OR_NAME_LIKE_BY_FUNCTIONS_CODES);
	Long countWhereCodeOrNameLikeByFunctionsCodes(QueryExecutorArguments arguments);
	
	/*
	String QUERY_IDENTIFIER_READ_WHERE_CODE_OR_NAME_LIKE = QueryIdentifierGetter.getInstance().get(ScopeFunction.class, QueryName.READ_WHERE_CODE_OR_NAME_LIKE);
	Collection<ScopeFunction> readWhereCodeOrNameLike(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_COUNT_WHERE_CODE_OR_NAME_LIKE = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_WHERE_CODE_OR_NAME_LIKE);
	Long countWhereCodeOrNameLike(QueryExecutorArguments arguments);
	*/
	String QUERY_IDENTIFIER_READ_BY_PARENTS_IDENTIFIERS = Querier.buildIdentifier(ScopeFunction.class, "readByParentsIdentifiers");
	Collection<ScopeFunction> readByParentsIdentifiers(Collection<String> parentsIdentifiers);
	
	String QUERY_IDENTIFIER_COUNT_BY_PARENTS_IDENTIFIERS = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_BY_PARENTS_IDENTIFIERS);
	Long countByParentsIdentifiers(Collection<String> parentsIdentifiers);
	
	String QUERY_IDENTIFIER_READ_BY_PARENTS_IDENTIFIERS_FOR_UI = QUERY_IDENTIFIER_READ_BY_PARENTS_IDENTIFIERS+"ForUI";
	Collection<ScopeFunction> readByParentsIdentifiersForUI(Collection<String> parentsIdentifiers);
	
	String QUERY_IDENTIFIER_READ_WITH_CODES_ONLY_BY_FUNCTIONS_IDENTIFIERS = Querier.buildIdentifier(ScopeFunction.class, "readWithCodesOnlyByFunctionsIdentifiers");
	Collection<ScopeFunction> readWithCodesOnlyByFunctionsIdentifiers(Collection<String> functionsIdentifiers);
	
	String QUERY_IDENTIFIER_READ_WHERE_FILTER = QueryIdentifierGetter.getInstance().get(ScopeFunction.class, QueryName.READ_WHERE_FILTER);
	Collection<ScopeFunction> readWhereFilter(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_COUNT_WHERE_FILTER = QueryIdentifierGetter.getInstance().get(ScopeFunction.class, QueryName.COUNT_WHERE_FILTER);
	Long countWhereFilter(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI = QueryIdentifierGetter.getInstance().get(ScopeFunction.class, QueryName.READ_WHERE_FILTER_FOR_UI);
	Collection<ScopeFunction> readWhereFilterForUI(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_UI = QueryIdentifierGetter.getInstance().get(ScopeFunction.class, QueryName.READ_BY_IDENTIFIER_FOR_UI);
	ScopeFunction readByIdentifierForUI(String identifier);
	
	Integer readMaxOrderNumberByFunctionCode(String functionCode);
	ScopeFunction readMaxCodeWhereCodeStartsWith(String string);
	Integer readMaxDocumentNumberByFunctionCode(String functionCode);
	
	Collection<Object[]> readFromViewByCodes(Collection<String> codes);
	Collection<Object[]> readFromViewByCodes(String...codes);
	Collection<Object[]> readFromViewByScopeFunctions(Collection<ScopeFunction> scopeFunctions);
	Collection<Object[]> readFromViewByScopeFunctions(ScopeFunction...scopeFunctions);
	
	Collection<Object[]> readFunctionsCodesByIdentifiers(Collection<String> identifiers);
	Collection<Object[]> readFunctionsCodesByIdentifiers(String...identifiers);
	Collection<Object[]> readFunctionsCodes(Collection<ScopeFunction> scopeFunctions);
	Collection<Object[]> readFunctionsCodes(ScopeFunction...scopeFunctions);
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<ScopeFunction> implements ScopeFunctionQuerier,Serializable {	
		
	}
	
	/**/
	
	static ScopeFunctionQuerier getInstance() {
		return Helper.getInstance(ScopeFunctionQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	/**/
	
	static void initialize() {
		Querier.CodableAndNamable.initialize(ScopeFunction.class);
		
		QueryManager.getInstance().register(
				Query.buildSelect(ScopeFunction.class, QueryIdentifierGetter.getInstance().get(ScopeFunction.class, QueryName.READ)
						, "SELECT sf FROM ScopeFunction sf ORDER BY sf.code ASC")
				,Query.buildCount(QueryIdentifierGetter.getInstance().get(ScopeFunction.class, QueryName.COUNT)
						, "SELECT COUNT(sf.identifier) FROM ScopeFunction sf")
				
				,Query.buildSelect(ScopeFunction.class, QUERY_IDENTIFIER_READ_BY_SCOPE_IDENTIFIER_BY_FUNCTION_CODE_FOR_UI
						, String.format("SELECT t.identifier,t.code,t.name FROM ScopeFunction t WHERE t.scope.identifier = :%s AND t.function.code = :%s ORDER BY t.code ASC"
								,PARAMETER_NAME_SCOPE_IDENTIFIER,PARAMETER_NAME_FUNCTION_CODE))
					.setTupleFieldsNamesIndexesFromFieldsNames(ScopeFunction.FIELD_IDENTIFIER,ScopeFunction.FIELD_CODE,ScopeFunction.FIELD_NAME)
				
				,Query.buildSelect(ScopeFunction.class, QUERY_IDENTIFIER_READ_ALL_WITH_REFERENCES_ONLY
						, "SELECT t.identifier,t.code,t.scope.identifier,t.scope.code,t.function.code,locality.identifier,locality.code,t.activityIdentifier FROM ScopeFunction t "
								+ "LEFT JOIN Locality locality ON locality = t.locality")
					.setTupleFieldsNamesIndexesFromFieldsNames(ScopeFunction.FIELD_IDENTIFIER,ScopeFunction.FIELD_CODE,ScopeFunction.FIELD_SCOPE_IDENTIFIER
							,ScopeFunction.FIELD_SCOPE_CODE,ScopeFunction.FIELD_FUNCTION_AS_STRING,ScopeFunction.FIELD_LOCALITY_IDENTIFIER,ScopeFunction.FIELD_LOCALITY_CODE
							,ScopeFunction.FIELD_ACTIVITY_IDENTIFIER)
				
				,Query.buildSelect(ScopeFunction.class, QUERY_IDENTIFIER_READ_BY_FUNCTIONS_IDENTIFIERS
						, "SELECT sf FROM ScopeFunction sf WHERE sf.function.identifier IN :"+PARAMETER_NAME_FUNCTIONS_IDENTIFIERS)
				,Query.buildCount(QUERY_IDENTIFIER_COUNT_BY_FUNCTIONS_IDENTIFIERS
						, "SELECT COUNT(sf) FROM ScopeFunction sf WHERE sf.function.identifier IN :"+PARAMETER_NAME_FUNCTIONS_IDENTIFIERS)
				
				,Query.buildSelect(ScopeFunction.class, QUERY_IDENTIFIER_READ_BY_SCOPE_TYPES_IDENTIFIERS_BY_FUNCTIONS_IDENTIFIERS
						, String.format("SELECT t FROM ScopeFunction t WHERE t.scope.type.identifier IN :%s AND t.function.identifier IN :%s ORDER BY t.function.code ASC,t.scope.code ASC"
								,PARAMETER_NAME_SCOPE_TYPES_IDENTIFIERS, PARAMETER_NAME_FUNCTIONS_IDENTIFIERS))
				
				,Query.buildSelect(ScopeFunction.class, QUERY_IDENTIFIER_READ_WHERE_CODIFICATION_DATE_IS_NULL_BY_SCOPE_TYPES_IDENTIFIERS_BY_FUNCTIONS_IDENTIFIERS
						, String.format("SELECT t FROM ScopeFunction t WHERE t.codificationDate IS NULL and t.scope.type.identifier IN :%s AND t.function.identifier IN :%s ORDER BY t.function.code ASC,t.scope.code ASC"
								,PARAMETER_NAME_SCOPE_TYPES_IDENTIFIERS, PARAMETER_NAME_FUNCTIONS_IDENTIFIERS))
				
				,Query.buildSelect(ScopeFunction.class, QUERY_IDENTIFIER_READ_CODES_NAMES_PARENTS_IDENTIFIERS_BY_IDENTIFIERS
						, "SELECT sf.identifier,sf.code,sf.name,sf.parentIdentifier FROM ScopeFunction sf WHERE sf.identifier IN :"+PARAMETER_NAME_IDENTIFIERS)
				.setTupleFieldsNamesIndexesFromFieldsNames(ScopeFunction.FIELD_IDENTIFIER,ScopeFunction.FIELD_CODE,ScopeFunction.FIELD_NAME,ScopeFunction.FIELD_PARENT_IDENTIFIER)
				
				,Query.buildSelect(ScopeFunction.class, QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_UI
						, "SELECT sf.identifier,sf.code,sf.name,sf.function.code,sf.parentIdentifier FROM ScopeFunction sf WHERE sf.identifier = :"+PARAMETER_NAME_IDENTIFIER)
				.setTupleFieldsNamesIndexesFromFieldsNames(ScopeFunction.FIELD_IDENTIFIER,ScopeFunction.FIELD_CODE,ScopeFunction.FIELD_NAME
						,ScopeFunction.FIELD_FUNCTION_CODE,ScopeFunction.FIELD_PARENT_IDENTIFIER)
				
				,Query.buildSelect(ScopeFunction.class, QUERY_IDENTIFIER_READ_BY_PARENTS_IDENTIFIERS
						, "SELECT sf.identifier,sf.code,sf.name,sf.parentIdentifier FROM ScopeFunction sf WHERE sf.parentIdentifier IN :"+PARAMETER_NAME_PARENTS_IDENTIFIERS)
				.setTupleFieldsNamesIndexesFromFieldsNames(ScopeFunction.FIELD_IDENTIFIER,ScopeFunction.FIELD_CODE,ScopeFunction.FIELD_NAME,ScopeFunction.FIELD_PARENT_IDENTIFIER)
				
				,Query.buildCount(QUERY_IDENTIFIER_COUNT_BY_PARENTS_IDENTIFIERS
						, "SELECT COUNT(sf.identifier) FROM ScopeFunction sf WHERE sf.parentIdentifier IN :"+PARAMETER_NAME_PARENTS_IDENTIFIERS)
				
				,Query.buildSelect(ScopeFunction.class, QUERY_IDENTIFIER_READ_BY_PARENTS_IDENTIFIERS_FOR_UI
						, "SELECT sf.identifier,sf.code,sf.name,sf.function.code,sf.parentIdentifier FROM ScopeFunction sf WHERE sf.parentIdentifier IN :"+PARAMETER_NAME_PARENTS_IDENTIFIERS+" ORDER BY sf.code ASC")
				.setTupleFieldsNamesIndexesFromFieldsNames(ScopeFunction.FIELD_IDENTIFIER,ScopeFunction.FIELD_CODE,ScopeFunction.FIELD_NAME
						,ScopeFunction.FIELD_FUNCTION_CODE,ScopeFunction.FIELD_PARENT_IDENTIFIER)
								
				,Query.buildSelect(ScopeFunction.class, QUERY_IDENTIFIER_READ_CODES_NAMES_BY_PARENTS_IDENTIFIERS
						, "SELECT sf.identifier,sf.code,sf.name,sf.parentIdentifier FROM ScopeFunction sf WHERE sf.parentIdentifier IN :"+PARAMETER_NAME_PARENTS_IDENTIFIERS+" ORDER BY sf.code ASC")
				.setTupleFieldsNamesIndexesFromFieldsNames(ScopeFunction.FIELD_IDENTIFIER,ScopeFunction.FIELD_CODE,ScopeFunction.FIELD_NAME,ScopeFunction.FIELD_PARENT_IDENTIFIER)
				
				,Query.buildSelect(ScopeFunction.class, QUERY_IDENTIFIER_READ_BY_FUNCTIONS_CODES
						, "SELECT sf FROM ScopeFunction sf WHERE sf.function.code IN :"+PARAMETER_NAME_FUNCTIONS_CODES)
				,Query.buildCount(QUERY_IDENTIFIER_COUNT_BY_FUNCTIONS_CODES
						, "SELECT COUNT(sf) FROM ScopeFunction sf WHERE sf.function.code IN :"+PARAMETER_NAME_FUNCTIONS_CODES)
				
				,Query.buildSelect(ScopeFunction.class, QUERY_IDENTIFIER_READ_WHERE_CODE_OR_NAME_LIKE_BY_FUNCTION_CODE
						, getQueryValueReadWhereCodeOrNameLikeByFunctionCode())
						.setTupleFieldsNamesIndexesFromFieldsNames(ScopeFunction.FIELD_IDENTIFIER,ScopeFunction.FIELD_CODE,ScopeFunction.FIELD_NAME)
				
				,Query.buildCount(QUERY_IDENTIFIER_COUNT_WHERE_CODE_OR_NAME_LIKE_BY_FUNCTION_CODE
						, getQueryValueCountWhereCodeOrNameLikeByFunctionCode())
				
				,Query.buildSelect(ScopeFunction.class, QUERY_IDENTIFIER_READ_WHERE_CODE_OR_NAME_LIKE_BY_FUNCTIONS_CODES
						, getQueryValueReadWhereCodeOrNameLikeByFunctionsCodes())
						.setTupleFieldsNamesIndexesFromFieldsNames(ScopeFunction.FIELD_IDENTIFIER,ScopeFunction.FIELD_CODE,ScopeFunction.FIELD_NAME)
				
				,Query.buildCount(QUERY_IDENTIFIER_COUNT_WHERE_CODE_OR_NAME_LIKE_BY_FUNCTIONS_CODES
						, getQueryValueCountWhereCodeOrNameLikeByFunctionsCodes())
				
				,Query.buildSelect(ScopeFunction.class, QUERY_IDENTIFIER_READ_WITH_CODES_ONLY_BY_FUNCTIONS_IDENTIFIERS
						, "SELECT sf.code,sf.scope.code,sf.function.code FROM ScopeFunction sf WHERE sf.function.identifier IN :"+PARAMETER_NAME_FUNCTIONS_IDENTIFIERS)
						.setTupleFieldsNamesIndexesFromFieldsNames(ScopeFunction.FIELD_CODE,ScopeFunction.FIELD_SCOPE_AS_STRING,ScopeFunction.FIELD_FUNCTION_AS_STRING)
				
				,Query.buildSelect(ScopeFunction.class, QUERY_IDENTIFIER_READ_WHERE_FILTER
						, getQueryValueReadWhereFilter()).setTupleFieldsNamesIndexesFromFieldsNames(ScopeFunction.FIELD_IDENTIFIER,ScopeFunction.FIELD_CODE
						,ScopeFunction.FIELD_NAME,ScopeFunction.FIELD_SHARED_AS_STRING,ScopeFunction.FIELD_SCOPE_AS_STRING,ScopeFunction.FIELD_FUNCTION_AS_STRING)
				
				,Query.buildSelect(ScopeFunction.class, QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI
						, getQueryValueReadWhereFilterForUI()).setTupleFieldsNamesIndexesFromFieldsNames(ScopeFunction.FIELD_IDENTIFIER,ScopeFunction.FIELD_CODE
						,ScopeFunction.FIELD_NAME,ScopeFunction.FIELD_SHARED_AS_STRING,ScopeFunction.FIELD_PARENT_IDENTIFIER,ScopeFunction.FIELD_SCOPE_AS_STRING
						,ScopeFunction.FIELD_FUNCTION_CODE,ScopeFunction.FIELD_FUNCTION_AS_STRING,ScopeFunction.FIELD_ACTORS_CODES)
				
				,Query.buildCount(QUERY_IDENTIFIER_COUNT_WHERE_FILTER, getQueryValueCountWhereFilter())
				
				/*
				,Query.buildSelect(ScopeFunction.class, QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI
					, getQueryValueReadWhereFilterForUI()).setTupleFieldsNamesIndexesFromFieldsNames(ScopeFunction.FIELD_IDENTIFIER,ScopeFunction.FIELD_CODE
					,ScopeFunction.FIELD_NAME,ScopeFunction.FIELD_SHARED_AS_STRING,ScopeFunction.FIELD_SCOPE_AS_STRING,ScopeFunction.FIELD_FUNCTION_AS_STRING)
				*/
		);
	}
	
	/**/
	
	static String getQueryValueReadWhereFilterFrom() {
		return jpql(
				From.ofTuple(ScopeFunction.class)
				,"LEFT JOIN Scope scope ON scope = t.scope"
				,"LEFT JOIN Function function ON function = t.function"
			);
	}
	
	static String getQueryValueReadWhereFilterWhere() {
		return where(and(
				String.format("(:%s = true OR t.function.identifier = :%s)", PARAMETER_NAME_FUNCTION_IDENTIFIER_NULLABLE,PARAMETER_NAME_FUNCTION_IDENTIFIER)
				,like("t", ScopeFunction.FIELD_CODE, PARAMETER_NAME_CODE)
				,like("t", ScopeFunction.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
				
				//,like("t", FieldHelper.join(ScopeFunction.FIELD_SCOPE,Function.FIELD_CODE), PARAMETER_NAME_SCOPE_CODE)
				//,like("t", FieldHelper.join(ScopeFunction.FIELD_SCOPE,Function.FIELD_NAME), PARAMETER_NAME_SCOPE_NAME,NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
				,parenthesis(or(
						like("t", FieldHelper.join(ScopeFunction.FIELD_SCOPE,Function.FIELD_CODE), PARAMETER_NAME_SCOPE_CODE_NAME)
						,like("t", FieldHelper.join(ScopeFunction.FIELD_SCOPE,Function.FIELD_NAME), PARAMETER_NAME_SCOPE_CODE_NAME,NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
						))
				//,like("t", FieldHelper.join(ScopeFunction.FIELD_SCOPE,Function.FIELD_NAME), PARAMETER_NAME_SCOPE_CODE_NAME,NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
				
				,like("t", FieldHelper.join(ScopeFunction.FIELD_FUNCTION,Function.FIELD_CODE), PARAMETER_NAME_FUNCTION_CODE)
			));
	}
	
	static String getQueryValueReadWhereFilterOrder() {
		return order(Order.join(asc("t",ScopeFunction.FIELD_CODE)));
	}
	
	static String getQueryValueReadWhereFilter() {
		return jpql(
				select(				
					Select.fields("t",ScopeFunction.FIELD_IDENTIFIER,ScopeFunction.FIELD_CODE,ScopeFunction.FIELD_NAME,ScopeFunction.FIELD_NUMBER_OF_ACTOR)
					,concatCodeName(ScopeFunction.FIELD_SCOPE),concatCodeName(ScopeFunction.FIELD_FUNCTION)
				)
				,getQueryValueReadWhereFilterFrom()
				,getQueryValueReadWhereFilterWhere()
				,getQueryValueReadWhereFilterOrder()
			);
	}
	
	static String getQueryValueReadWhereFilterForUI() {
		return jpql(
				select(				
					Select.fields("t",ScopeFunction.FIELD_IDENTIFIER,ScopeFunction.FIELD_CODE,ScopeFunction.FIELD_NAME,ScopeFunction.FIELD_NUMBER_OF_ACTOR,ScopeFunction.FIELD_PARENT_IDENTIFIER)
					,Select.concatCodeName(ScopeFunction.FIELD_SCOPE),"function.code",Select.concatCodeName(ScopeFunction.FIELD_FUNCTION)
					,Select.fields("t",ScopeFunction.FIELD_ACTORS_CODES)
				)
				,getQueryValueReadWhereFilterFrom()
				,getQueryValueReadWhereFilterWhere()
				,getQueryValueReadWhereFilterOrder()
			);
	}
	
	static String getQueryValueCountWhereFilter() {
		return jpql(select("COUNT(t.identifier)"),From.ofTuple(ScopeFunction.class),getQueryValueReadWhereFilterWhere());
	}
	
	static String getQueryValueReadWhereCodeOrNameLikeByFunctionCodeFromWhere() {
		return jpql(
				from("ScopeFunction t")
				,where(and(
					parenthesis(or(
						like("t", ScopeFunction.FIELD_CODE, PARAMETER_NAME_CODE)
						,like("t", ScopeFunction.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
					))
					,"t.function.code = :"+PARAMETER_NAME_FUNCTION_CODE
				))
			);
	}
	
	static String getQueryValueReadWhereCodeOrNameLikeByFunctionCode() {
		return jpql(select("t.identifier","t.code","t.name"),getQueryValueReadWhereCodeOrNameLikeByFunctionCodeFromWhere(),order("t.code ASC"));
	}
	
	
	static String getQueryValueCountWhereCodeOrNameLikeByFunctionCode() {
		return jpql(select("COUNT(t.identifier)"),getQueryValueReadWhereCodeOrNameLikeByFunctionCodeFromWhere());
	}
	
	static String getQueryValueReadWhereCodeOrNameLikeByFunctionsCodesFromWhere() {
		return jpql(
				from("ScopeFunction t")
				,where(and(
					parenthesis(or(
						like("t", ScopeFunction.FIELD_CODE, PARAMETER_NAME_CODE)
						,like("t", ScopeFunction.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
					))
					,"t.function.code IN :"+PARAMETER_NAME_FUNCTIONS_CODES
				))
			);
	}
	
	static String getQueryValueReadWhereCodeOrNameLikeByFunctionsCodes() {
		return jpql(select("t.identifier","t.code","t.name"),getQueryValueReadWhereCodeOrNameLikeByFunctionsCodesFromWhere(),order("t.code ASC"));
	}
	
	
	static String getQueryValueCountWhereCodeOrNameLikeByFunctionsCodes() {
		return jpql(select("COUNT(t.identifier)"),getQueryValueReadWhereCodeOrNameLikeByFunctionsCodesFromWhere());
	}
}