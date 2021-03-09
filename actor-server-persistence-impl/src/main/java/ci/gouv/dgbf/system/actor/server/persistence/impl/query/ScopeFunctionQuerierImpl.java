package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.array.ArrayHelper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.persistence.EntityManagerGetter;
import org.cyk.utility.persistence.query.Filter;
import org.cyk.utility.persistence.query.QueryExecutor;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryName;
import org.cyk.utility.persistence.server.query.ReaderByCollection;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

public class ScopeFunctionQuerierImpl extends ScopeFunctionQuerier.AbstractImpl implements Serializable {

	@Override
	public ScopeFunction readOne(QueryExecutorArguments arguments) {
		if(arguments != null && arguments.getQuery() != null && QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_UI.equals(arguments.getQuery().getIdentifier()))
			return readByIdentifierForUI((String) arguments.getFilterFieldValue(PARAMETER_NAME_IDENTIFIER));
		return super.readOne(arguments);
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public Collection<ScopeFunction> readMany(QueryExecutorArguments arguments) {
		if(arguments != null && arguments.getQuery() != null && QUERY_IDENTIFIER_READ_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
			return readWhereFilter(arguments);
		if(arguments != null && arguments.getQuery() != null && QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI.equals(arguments.getQuery().getIdentifier()))
			return readWhereFilterForUI(arguments);
		if(arguments != null && arguments.getQuery() != null && QUERY_IDENTIFIER_READ_BY_FUNCTIONS_CODES.equals(arguments.getQuery().getIdentifier()))
			return readByFunctionsCodes(arguments);
		if(arguments != null && arguments.getQuery() != null && QUERY_IDENTIFIER_READ_BY_PARENTS_IDENTIFIERS_FOR_UI.equals(arguments.getQuery().getIdentifier()))
			return readByParentsIdentifiersForUI((Collection<String>) arguments.getFilterFieldValue(PARAMETER_NAME_PARENTS_IDENTIFIERS));
		if(arguments != null && arguments.getQuery() != null && QUERY_IDENTIFIER_READ_WHERE_CODE_OR_NAME_LIKE_BY_FUNCTION_CODE.equals(arguments.getQuery().getIdentifier()))
			return readWhereCodeOrNameLikeByFunctionCode(arguments);
		if(arguments != null && arguments.getQuery() != null && QUERY_IDENTIFIER_READ_WHERE_CODE_OR_NAME_LIKE_BY_FUNCTIONS_CODES.equals(arguments.getQuery().getIdentifier()))
			return readWhereCodeOrNameLikeByFunctionsCodes(arguments);
		if(arguments != null && arguments.getQuery() != null && QUERY_IDENTIFIER_READ_BY_SCOPE_IDENTIFIER_BY_FUNCTION_CODE_FOR_UI.equals(arguments.getQuery().getIdentifier()))
			return readByScopeIdentifierByFunctionCodeForUI(arguments);
		return super.readMany(arguments);
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public Long count(QueryExecutorArguments arguments) {
		if(arguments != null && arguments.getQuery() != null && QUERY_IDENTIFIER_COUNT_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
			return countWhereFilter(arguments);
		if(arguments != null && arguments.getQuery() != null && QUERY_IDENTIFIER_COUNT_BY_FUNCTIONS_CODES.equals(arguments.getQuery().getIdentifier()))
			return countByFunctionsCodes(arguments);
		if(arguments != null && arguments.getQuery() != null && QUERY_IDENTIFIER_COUNT_WHERE_CODE_OR_NAME_LIKE_BY_FUNCTION_CODE.equals(arguments.getQuery().getIdentifier()))
			return countWhereCodeOrNameLikeByFunctionCode(arguments);
		if(arguments != null && arguments.getQuery() != null && QUERY_IDENTIFIER_COUNT_WHERE_CODE_OR_NAME_LIKE_BY_FUNCTIONS_CODES.equals(arguments.getQuery().getIdentifier()))
			return countWhereCodeOrNameLikeByFunctionsCodes(arguments);
		if(arguments != null && arguments.getQuery() != null && QUERY_IDENTIFIER_COUNT_BY_PARENTS_IDENTIFIERS.equals(arguments.getQuery().getIdentifier()))
			return countByParentsIdentifiers((Collection<String>) arguments.getFilterFieldValue(PARAMETER_NAME_PARENTS_IDENTIFIERS));
		return super.count( arguments);
	}
	
	@Override
	public Collection<ScopeFunction> read() {
		return super.read(ScopeFunction.class, QueryExecutorArguments.instantiate(ScopeFunction.class, QueryName.READ));
	}
	
	@Override
	public Collection<ScopeFunction> readByParentsIdentifiers(Collection<String> parentsIdentifiers) {
		return QueryExecutor.getInstance().executeReadMany(ScopeFunction.class, QUERY_IDENTIFIER_READ_BY_PARENTS_IDENTIFIERS
				,PARAMETER_NAME_PARENTS_IDENTIFIERS,parentsIdentifiers);
	}
	
	@Override
	public Long countByParentsIdentifiers(Collection<String> parentsIdentifiers) {
		return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT_BY_PARENTS_IDENTIFIERS
				,PARAMETER_NAME_PARENTS_IDENTIFIERS,parentsIdentifiers);
	}
	
	@Override
	public Collection<ScopeFunction> readByParentsIdentifiersForUI(Collection<String> parentsIdentifiers) {
		Collection<ScopeFunction> scopeFunctions = QueryExecutor.getInstance().executeReadMany(ScopeFunction.class, QUERY_IDENTIFIER_READ_BY_PARENTS_IDENTIFIERS_FOR_UI
				,PARAMETER_NAME_PARENTS_IDENTIFIERS,parentsIdentifiers);
		listenReadForUI(scopeFunctions);
		return scopeFunctions;
	}
	
	@Override
	public Collection<ScopeFunction> readCodesNamesParentsIdentifiersByIdentifiers(Collection<String> identifiers) {
		return new ReaderByCollection.AbstractImpl<String, ScopeFunction>(){
			@Override
			protected Collection<ScopeFunction> __read__(Collection<String> values) {
				return QueryExecutor.getInstance().executeReadMany(ScopeFunction.class, QUERY_IDENTIFIER_READ_CODES_NAMES_PARENTS_IDENTIFIERS_BY_IDENTIFIERS
						,PARAMETER_NAME_IDENTIFIERS,values);
			}				
		}.read(identifiers);	
		//return QueryExecutor.getInstance().executeReadMany(ScopeFunction.class, QUERY_IDENTIFIER_READ_CODES_NAMES_PARENTS_IDENTIFIERS_BY_IDENTIFIERS
		//		,PARAMETER_NAME_IDENTIFIERS,identifiers);
	}
	
	@Override
	public Collection<ScopeFunction> readCodesNamesByParentsIdentifiers(Collection<String> identifiers) {
		return new ReaderByCollection.AbstractImpl<String, ScopeFunction>(){
			@Override
			protected Collection<ScopeFunction> __read__(Collection<String> values) {
				return QueryExecutor.getInstance().executeReadMany(ScopeFunction.class, QUERY_IDENTIFIER_READ_CODES_NAMES_BY_PARENTS_IDENTIFIERS
						,PARAMETER_NAME_PARENTS_IDENTIFIERS,values);
			}				
		}.read(identifiers);			
		//return QueryExecutor.getInstance().executeReadMany(ScopeFunction.class, QUERY_IDENTIFIER_READ_CODES_NAMES_BY_PARENTS_IDENTIFIERS
		//		,PARAMETER_NAME_PARENTS_IDENTIFIERS,identifiers);
	}
	
	@Override
	public Collection<ScopeFunction> readByScopeIdentifierByFunctionCodeForUI(QueryExecutorArguments arguments) {
		String scopeIdentifier = (String)arguments.getFilterFieldValue(PARAMETER_NAME_SCOPE_IDENTIFIER);
		if(StringHelper.isBlank(scopeIdentifier))
			return null;
		String functionCode = (String)arguments.getFilterFieldValue(PARAMETER_NAME_FUNCTION_CODE);
		if(StringHelper.isBlank(functionCode))
			return null;
		Collection<ScopeFunction> scopeFunctions = QueryExecutor.getInstance().executeReadMany(ScopeFunction.class, QUERY_IDENTIFIER_READ_BY_SCOPE_IDENTIFIER_BY_FUNCTION_CODE_FOR_UI
				,PARAMETER_NAME_SCOPE_IDENTIFIER,scopeIdentifier,PARAMETER_NAME_FUNCTION_CODE,functionCode);
		return scopeFunctions;
	}
	
	@Override
	public Collection<ScopeFunction> readWithCodesOnlyByFunctionsIdentifiers(Collection<String> functionsIdentifiers) {
		return QueryExecutor.getInstance().executeReadMany(ScopeFunction.class, QUERY_IDENTIFIER_READ_WITH_CODES_ONLY_BY_FUNCTIONS_IDENTIFIERS
				,PARAMETER_NAME_FUNCTIONS_IDENTIFIERS,functionsIdentifiers);
	}
	
	@Override
	public Collection<ScopeFunction> readByFunctionsIdentifiers(Collection<String> functionsIdentifiers) {
		return QueryExecutor.getInstance().executeReadMany(ScopeFunction.class, QUERY_IDENTIFIER_READ_BY_FUNCTIONS_IDENTIFIERS
				,PARAMETER_NAME_FUNCTIONS_IDENTIFIERS,functionsIdentifiers);
	}
	
	@Override
	public Long countByFunctionsIdentifiers(Collection<String> functionsIdentifiers) {
		return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT_BY_FUNCTIONS_IDENTIFIERS
				,PARAMETER_NAME_FUNCTIONS_IDENTIFIERS,functionsIdentifiers);
	}
	
	@Override
	public Collection<ScopeFunction> readByScopeTypesIdentifiersByFunctionsIdentifiers(Collection<String> scopeTypesIdentifiers, Collection<String> functionsIdentifiers) {
		return QueryExecutor.getInstance().executeReadMany(ScopeFunction.class, QUERY_IDENTIFIER_READ_BY_SCOPE_TYPES_IDENTIFIERS_BY_FUNCTIONS_IDENTIFIERS
				,PARAMETER_NAME_SCOPE_TYPES_IDENTIFIERS,scopeTypesIdentifiers,PARAMETER_NAME_FUNCTIONS_IDENTIFIERS,functionsIdentifiers);
	}
	
	@Override
	public Collection<ScopeFunction> readWhereCodificationDateIsNullByScopeTypesIdentifiersByFunctionsIdentifiers(Collection<String> scopeTypesIdentifiers, Collection<String> functionsIdentifiers) {
		return QueryExecutor.getInstance().executeReadMany(ScopeFunction.class, QUERY_IDENTIFIER_READ_WHERE_CODIFICATION_DATE_IS_NULL_BY_SCOPE_TYPES_IDENTIFIERS_BY_FUNCTIONS_IDENTIFIERS
				,PARAMETER_NAME_SCOPE_TYPES_IDENTIFIERS,scopeTypesIdentifiers,PARAMETER_NAME_FUNCTIONS_IDENTIFIERS,functionsIdentifiers);
	}
	
	@Override
	public Collection<ScopeFunction> readByFunctionsCodes(QueryExecutorArguments arguments) {
		return QueryExecutor.getInstance().executeReadMany(ScopeFunction.class, arguments);
	}
	
	@Override
	public Collection<ScopeFunction> readByFunctionsCodes(Collection<String> functionsCodes) {
		if(CollectionHelper.isEmpty(functionsCodes))
			return null;
		return readByFunctionsCodes(new QueryExecutorArguments().setQueryFromIdentifier(QUERY_IDENTIFIER_READ_BY_FUNCTIONS_CODES)
				.addFilterFieldsValues(PARAMETER_NAME_FUNCTIONS_CODES,functionsCodes));
	}
	
	@Override
	public Collection<ScopeFunction> readByFunctionsCodes(String... functionsCodes) {
		if(ArrayHelper.isEmpty(functionsCodes))
			return null;
		return readByFunctionsCodes(CollectionHelper.listOf(functionsCodes));
	}
	
	@Override
	public Long countByFunctionsCodes(QueryExecutorArguments arguments) {
		return QueryExecutor.getInstance().executeCount(arguments);
	}
	
	@Override
	public Long countByFunctionsCodes(Collection<String> functionsCodes) {
		if(CollectionHelper.isEmpty(functionsCodes))
			return null;
		return countByFunctionsCodes(new QueryExecutorArguments().setQueryFromIdentifier(QUERY_IDENTIFIER_COUNT_BY_FUNCTIONS_CODES)
				.addFilterFieldsValues(PARAMETER_NAME_FUNCTIONS_CODES,functionsCodes));
	}
	
	@Override
	public Long countByFunctionsCodes(String... functionsCodes) {
		if(ArrayHelper.isEmpty(functionsCodes))
			return null;
		return countByFunctionsCodes(CollectionHelper.listOf(functionsCodes));
	}
	
	//
	
	@Override
	public Collection<ScopeFunction> readWhereCodeOrNameLikeByFunctionCode(QueryExecutorArguments arguments) {
		if(arguments == null)
			arguments = new QueryExecutorArguments();
		if(arguments.getQuery() == null)
			arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_WHERE_CODE_OR_NAME_LIKE_BY_FUNCTION_CODE);
		prepareWhereCodeOrNameLikeByFunctionCode(arguments);
		return QueryExecutor.getInstance().executeReadMany(ScopeFunction.class, arguments);
	}
	
	@Override
	public Long countWhereCodeOrNameLikeByFunctionCode(QueryExecutorArguments arguments) {
		if(arguments == null)
			arguments = new QueryExecutorArguments();
		if(arguments.getQuery() == null)
			arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_COUNT_WHERE_CODE_OR_NAME_LIKE_BY_FUNCTION_CODE);
		prepareWhereCodeOrNameLikeByFunctionCode(arguments);
		return QueryExecutor.getInstance().executeCount(arguments);
	}
	
	protected void prepareWhereCodeOrNameLikeByFunctionCode(QueryExecutorArguments arguments) {
		Filter filter = new Filter();
		filter.addFieldContains(PARAMETER_NAME_CODE, arguments);
		filter.addFieldContainsStringOrWords(PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
		filter.addFieldEquals(PARAMETER_NAME_FUNCTION_CODE, arguments);
		arguments.setFilter(filter);
	}
	
	@Override
	public Collection<ScopeFunction> readWhereCodeOrNameLikeByFunctionsCodes(QueryExecutorArguments arguments) {
		if(arguments == null)
			arguments = new QueryExecutorArguments();
		if(arguments.getQuery() == null)
			arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_WHERE_CODE_OR_NAME_LIKE_BY_FUNCTIONS_CODES);
		prepareWhereCodeOrNameLikeByFunctionsCodes(arguments);
		return QueryExecutor.getInstance().executeReadMany(ScopeFunction.class, arguments);
	}
	
	@Override
	public Long countWhereCodeOrNameLikeByFunctionsCodes(QueryExecutorArguments arguments) {
		if(arguments == null)
			arguments = new QueryExecutorArguments();
		if(arguments.getQuery() == null)
			arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_COUNT_WHERE_CODE_OR_NAME_LIKE_BY_FUNCTIONS_CODES);
		prepareWhereCodeOrNameLikeByFunctionsCodes(arguments);
		return QueryExecutor.getInstance().executeCount(arguments);
	}
	
	protected void prepareWhereCodeOrNameLikeByFunctionsCodes(QueryExecutorArguments arguments) {
		Filter filter = new Filter();
		filter.addFieldContains(PARAMETER_NAME_CODE, arguments);
		filter.addFieldContainsStringOrWords(PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
		filter.addField(PARAMETER_NAME_FUNCTIONS_CODES, arguments.getFilterFieldValue(PARAMETER_NAME_FUNCTIONS_CODES));
		arguments.setFilter(filter);
	}
	
	//
	
	@Override
	public Collection<ScopeFunction> readWhereFilter(QueryExecutorArguments arguments) {
		if(arguments == null)
			arguments = QueryExecutorArguments.instantiate(ScopeFunction.class, QueryName.READ_WHERE_FILTER);
		prepareWhereFilter(arguments);
		return QueryExecutor.getInstance().executeReadMany(ScopeFunction.class, arguments);
	}
	
	@Override
	public Long countWhereFilter(QueryExecutorArguments arguments) {
		if(arguments == null)
			arguments = QueryExecutorArguments.instantiate(ScopeFunction.class, QueryName.COUNT_WHERE_FILTER);
		prepareWhereFilter(arguments);
		return QueryExecutor.getInstance().executeCount(arguments);
	}
	
	private void prepareWhereFilter(QueryExecutorArguments arguments) {
		Filter filter = new Filter();
		filter.addFieldsEquals(arguments, PARAMETER_NAME_FUNCTION_IDENTIFIER);
		filter.addFieldsNullable(arguments, PARAMETER_NAME_FUNCTION_IDENTIFIER);			
		filter.addFieldContains(PARAMETER_NAME_CODE, arguments);
		filter.addFieldContainsStringOrWords(PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
		filter.addFieldContains(PARAMETER_NAME_FUNCTION_CODE, arguments);
		
		//filter.addFieldContains(PARAMETER_NAME_SCOPE_CODE, arguments);
		filter.addFieldContainsStringOrWords(PARAMETER_NAME_SCOPE_CODE_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
		arguments.setFilter(filter);
	}
	
	@Override
	public Collection<ScopeFunction> readWhereFilterForUI(QueryExecutorArguments arguments) {
		if(arguments == null)
			arguments = QueryExecutorArguments.instantiate(ScopeFunction.class, QueryName.READ_WHERE_FILTER_FOR_UI);
		if(arguments.getQuery() == null)
			arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI);
		prepareWhereFilter(arguments);
		Collection<ScopeFunction> scopeFunctions = QueryExecutor.getInstance().executeReadMany(ScopeFunction.class, arguments);
		if(CollectionHelper.isEmpty(scopeFunctions))
			return null;
		listenReadForUI(scopeFunctions);
		return scopeFunctions;
	}
	
	private void listenReadForUI(Collection<ScopeFunction> scopeFunctions) {
		if(CollectionHelper.isEmpty(scopeFunctions))
			return;
		Collection<ScopeFunction> scopeFunctionsHolders = scopeFunctions.stream().filter(x -> Function.EXECUTION_HOLDERS_CODES.contains(x.getFunctionCode())).collect(Collectors.toList());
		if(CollectionHelper.isNotEmpty(scopeFunctionsHolders)) {
			Collection<ScopeFunction> assistants = readCodesNamesByParentsIdentifiers(FieldHelper.readSystemIdentifiersAsStrings(scopeFunctionsHolders));
			if(CollectionHelper.isNotEmpty(assistants))
				scopeFunctionsHolders.forEach(x -> x.setChildrenCodesNames(assistants.stream().filter(c -> x.getIdentifier().equals(c.getParentIdentifier()))
						.map(c -> c.toString()).collect(Collectors.toList())));
		}
		Collection<ScopeFunction> scopeFunctionsAssistants = scopeFunctions.stream().filter(x -> Function.EXECUTION_ASSISTANTS_CODES.contains(x.getFunctionCode())).collect(Collectors.toList());
		if(CollectionHelper.isNotEmpty(scopeFunctionsAssistants)) {
			Collection<String> holdersIdentifiers = scopeFunctionsAssistants.stream().map(x -> x.getParentIdentifier())
					.filter(x -> StringHelper.isNotBlank(x)).collect(Collectors.toList());
			if(CollectionHelper.isNotEmpty(holdersIdentifiers)) {
				Collection<ScopeFunction> holders = readCodesNamesParentsIdentifiersByIdentifiers(holdersIdentifiers);
				if(CollectionHelper.isNotEmpty(holders)) {
					for(ScopeFunction assistant : scopeFunctionsAssistants) {						
						for(ScopeFunction holder : holders)
							if(holder.getIdentifier().equals(assistant.getParentIdentifier())) {
								assistant.setParentAsString(holder.toString());
								break;
							}
					}
				}
			}					
		}
	}
	
	private void listenReadForUI(ScopeFunction scopeFunction) {
		if(scopeFunction == null)
			return;
		listenReadForUI(List.of(scopeFunction));
	}
	
	@Override
	public Collection<ScopeFunction> readAllWithReferencesOnly(QueryExecutorArguments arguments) {
		if(arguments == null)
			arguments = new QueryExecutorArguments();
		if(arguments.getQuery() == null)
			arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_ALL_WITH_REFERENCES_ONLY);
		return QueryExecutor.getInstance().executeReadMany(ScopeFunction.class, arguments);
	}
	
	@Override
	public ScopeFunction readByIdentifierForUI(String identifier) {
		if(StringHelper.isBlank(identifier))
			return null;
		ScopeFunction scopeFunction = QueryExecutor.getInstance().executeReadOne(ScopeFunction.class, new QueryExecutorArguments()
				.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_UI).addFilterField(PARAMETER_NAME_IDENTIFIER,identifier));
		listenReadForUI(scopeFunction);
		return scopeFunction;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public Collection<Object[]> readFromViewByCodes(Collection<String> codes) {
		if(CollectionHelper.isEmpty(codes))
			return null;
		return new ReaderByCollection.AbstractImpl<String, Object[]>() {
			@Override
			protected Collection<Object[]> __read__(Collection<String> values) {
				return EntityManagerGetter.getInstance().get().createNativeQuery("SELECT poste,email,civilite_nom_prenoms FROM V_APP_EX_FB WHERE poste IN :codes")
						.setParameter("codes", values).getResultList();
			}
			
		}.read(codes);
		//return EntityManagerGetter.getInstance().get().createNativeQuery("SELECT poste,email,civilite_nom_prenoms FROM V_APP_EX_FB WHERE poste IN :codes")
		//		.setParameter("codes", codes).getResultList();
	}
	
	@Override
	public Collection<Object[]> readFromViewByCodes(String... codes) {
		if(ArrayHelper.isEmpty(codes))
			return null;
		return readFromViewByCodes(CollectionHelper.listOf(codes));
	}
	
	@Override
	public Collection<Object[]> readFromViewByScopeFunctions(Collection<ScopeFunction> scopeFunctions) {
		if(CollectionHelper.isEmpty(scopeFunctions))
			return null;
		return readFromViewByCodes(scopeFunctions.stream().filter(x -> StringHelper.isNotBlank(x.getCode())).map(x -> x.getCode()).collect(Collectors.toList()));
	}
	
	@Override
	public Collection<Object[]> readFromViewByScopeFunctions(ScopeFunction... scopeFunctions) {
		if(ArrayHelper.isEmpty(scopeFunctions))
			return null;
		return readFromViewByScopeFunctions(CollectionHelper.listOf(scopeFunctions));
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public Collection<Object[]> readFunctionsCodesByIdentifiers(Collection<String> identifiers) {
		if(CollectionHelper.isEmpty(identifiers))
			return null;
		return new ReaderByCollection.AbstractImpl<String, Object[]>() {
			@Override
			protected Collection<Object[]> __read__(Collection<String> values) {
				return EntityManagerGetter.getInstance().get().createQuery("SELECT sf.identifier,sf.function.code FROM ScopeFunction sf WHERE sf.identifier IN :identifiers")
						.setParameter("identifiers", values).getResultList();
			}
			
		}.read(identifiers);
	}
	
	@Override
	public Collection<Object[]> readFunctionsCodesByIdentifiers(String... identifiers) {
		if(ArrayHelper.isEmpty(identifiers))
			return null;
		return readFunctionsCodesByIdentifiers(CollectionHelper.listOf(identifiers));
	}
	
	@Override
	public Collection<Object[]> readFunctionsCodes(Collection<ScopeFunction> scopeFunctions) {
		if(CollectionHelper.isEmpty(scopeFunctions))
			return null;
		return readFunctionsCodesByIdentifiers(FieldHelper.readSystemIdentifiersAsStrings(scopeFunctions));
	}
	
	@Override
	public Collection<Object[]> readFunctionsCodes(ScopeFunction... scopeFunctions) {
		if(ArrayHelper.isEmpty(scopeFunctions))
			return null;
		return readFunctionsCodes(CollectionHelper.listOf(scopeFunctions));
	}
	
	@Override
	protected Class<ScopeFunction> getKlass() {
		return ScopeFunction.class;
	}
	
}
