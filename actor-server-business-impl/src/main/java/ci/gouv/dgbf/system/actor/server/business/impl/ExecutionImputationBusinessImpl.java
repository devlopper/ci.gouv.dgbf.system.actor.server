package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import javax.enterprise.context.ApplicationScoped;
import javax.transaction.Transactional;

import org.cyk.utility.__kernel__.business.NativeQueryStringExecutor;
import org.cyk.utility.__kernel__.business.TransactionResult;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.enumeration.Action;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.log.LogHelper;
import org.cyk.utility.__kernel__.number.NumberHelper;
import org.cyk.utility.__kernel__.persistence.PersistenceHelper;
import org.cyk.utility.__kernel__.persistence.query.NativeQueryStringBuilder;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.filter.Filter;
import org.cyk.utility.__kernel__.runnable.Executor;
import org.cyk.utility.__kernel__.runnable.RunnableHelper;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.throwable.ThrowableHelper;
import org.cyk.utility.__kernel__.time.TimeHelper;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.ExecutionImputationBusiness;
import ci.gouv.dgbf.system.actor.server.business.api.ScopeFunctionExecutionImputationBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ExecutionImputationPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ExecutionImputationQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputationScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunctionExecutionImputation;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

@ApplicationScoped
public class ExecutionImputationBusinessImpl extends AbstractBusinessEntityImpl<ExecutionImputation, ExecutionImputationPersistence> implements ExecutionImputationBusiness,Serializable {
	private static final long serialVersionUID = 1L;

	@Override @Transactional
	public TransactionResult saveScopeFunctions(Collection<ExecutionImputation> executionImputations) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("execution imputations", executionImputations);
		LogHelper.logInfo(String.format("Enregistrement des affectations de %s imputation(s) en cours...",executionImputations.size()), getClass());
		ExecutionImputationQuerier.refreshMaterializedView();
		TransactionResult result = new TransactionResult().setTupleName("affectation");
		new Executor<SaveScopeFunctionsNativeQueryStringManyBuilderAndExecutor>().setName("Générateur et exécuteur de requête SQL").setNumberOfRunnablesToBeExecuted(1)
		.addRunnables(new SaveScopeFunctionsNativeQueryStringManyBuilderAndExecutor().setExecutionImputations(executionImputations).setResult(result))
		.run();
		if(NumberHelper.isGreaterThanZero(result.getNumberOfCreation()) || NumberHelper.isGreaterThanZero(result.getNumberOfUpdate()) || NumberHelper.isGreaterThanZero(result.getNumberOfDeletion()))
		ExecutionImputationQuerier.refreshMaterializedView();
		result.log(getClass());
		return result;
	}
	
	@Override @Transactional
	public TransactionResult deriveFromScopeFunctions(Collection<ScopeFunction> scopeFunctions) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("scope functions", scopeFunctions);
		LogHelper.logInfo(String.format("Assignation des imputations à partir de "+scopeFunctions.size()+" poste(s)"), getClass());
		return null;
	}

	@Override
	public TransactionResult deriveFromAllScopeFunctions() {
		return null;
	}		
	/*
	@Override
	public TransactionResult deriveScopeFunctionsFromModel(ExecutionImputation executionImputationModel,Filter filter) {
		ThrowableHelper.throwIllegalArgumentExceptionIfNull("executionImputationModel", executionImputationModel);
		ThrowableHelper.throwIllegalArgumentExceptionIfNull("filter", filter);
		ExecutionImputationQuerier.refreshMaterializedView();
		TransactionResult result = new TransactionResult();
		LogHelper.logInfo(String.format("Modèle : CM=%s|AO=%s|FC=%s|A=%s , filtre : %s", executionImputationModel.getCreditManager()
				,executionImputationModel.getAuthorizingOfficer(),executionImputationModel.getFinancialController(),executionImputationModel.getAccounting(),filter), getClass());
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments();
		queryExecutorArguments.setFilter(filter);
		queryExecutorArguments.setQueryFromIdentifier(ExecutionImputationQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER);		
		LogHelper.logInfo(String.format("Compte des imputations à traiter en cours..."), getClass());
		Long t = System.currentTimeMillis();
		Long numberOfExecutionImputations = ExecutionImputationQuerier.getInstance().countWhereFilter(queryExecutorArguments);
		LogHelper.logInfo(String.format("%s imputation(s) à traiter compté en %s", numberOfExecutionImputations,TimeHelper.formatDuration(System.currentTimeMillis() - t)), getClass());
		if(NumberHelper.isLessThanOrEqualZero(numberOfExecutionImputations))
			return null;
		t = System.currentTimeMillis();
		LogHelper.logInfo(String.format("Chargement des imputations(%s) à traiter en cours...",numberOfExecutionImputations), getClass());
		queryExecutorArguments.setQueryFromIdentifier(ExecutionImputationQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER);
		List<ExecutionImputation> executionImputations = (List<ExecutionImputation>) ExecutionImputationQuerier.getInstance().readWhereFilter(queryExecutorArguments);	
		LogHelper.logInfo(String.format("%s imputation(s) à traiter chargée(s) en %s", numberOfExecutionImputations,TimeHelper.formatDuration(System.currentTimeMillis() - t)), getClass());
		
		Integer batchSize = 250;
		Integer numberOfBatches = (int) (numberOfExecutionImputations / batchSize) + (numberOfExecutionImputations % batchSize == 0 ? 0 : 1);		
		LogHelper.logInfo(String.format("taille du lot est de %s. %s lot(s) à traiter",batchSize,numberOfBatches), getClass());				
		
		Executor<SaveScopeFunctionsNativeQueryStringManyBuilderAndExecutor> producer = new Executor<SaveScopeFunctionsNativeQueryStringManyBuilderAndExecutor>()
				.setName("Génération de requête SQL").setNumberOfRunnablesToBeExecuted(numberOfBatches)
				.setExecutorService(RunnableHelper.instantiateExecutorService(3,5,1l,TimeUnit.MINUTES,null,numberOfBatches,null,null));				
		producer.start();		
		for(Integer index = 0; index < numberOfBatches; index = index + 1) {
			Integer from = index * batchSize;
			Integer to = from + batchSize;
			if(to > executionImputations.size())
				to = executionImputations.size();
			Collection<ExecutionImputation> executionImputationsBatch = executionImputations.subList(from, to);			
			if(CollectionHelper.isEmpty(executionImputationsBatch))
				continue;	
			producer.addRunnables(new SaveScopeFunctionsNativeQueryStringManyBuilderAndExecutor().setExecutionImputationModel(executionImputationModel)
					.setExecutionImputations(executionImputationsBatch));			
		}
		producer.join();
		ExecutionImputationQuerier.refreshMaterializedView();
		executionImputations.clear();
		executionImputations = null;
		result.log(getClass());
		return result;
	}*/
	
	@Override
	public TransactionResult deriveScopeFunctionsFromModel(ExecutionImputation executionImputationModel,Filter filter) {
		ThrowableHelper.throwIllegalArgumentExceptionIfNull("executionImputationModel", executionImputationModel);
		ThrowableHelper.throwIllegalArgumentExceptionIfNull("filter", filter);
		ExecutionImputationQuerier.refreshMaterializedView();
		LogHelper.logInfo(String.format("Application de modèle : CM=%s|AO=%s|FC=%s|A=%s , filtre : %s", executionImputationModel.getCreditManager()
				,executionImputationModel.getAuthorizingOfficer(),executionImputationModel.getFinancialController(),executionImputationModel.getAccounting(),filter), getClass());
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments();
		queryExecutorArguments.setFilter(filter);
		queryExecutorArguments.setQueryFromIdentifier(ExecutionImputationQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER);		
		LogHelper.logInfo(String.format("Compte des imputations à traiter en cours..."), getClass());
		Long t = System.currentTimeMillis();
		Long numberOfExecutionImputations = ExecutionImputationQuerier.getInstance().countWhereFilter(queryExecutorArguments);
		LogHelper.logInfo(String.format("%s imputation(s) à traiter compté en %s", numberOfExecutionImputations,TimeHelper.formatDuration(System.currentTimeMillis() - t)), getClass());
		if(NumberHelper.isLessThanOrEqualZero(numberOfExecutionImputations))
			return null;
		t = System.currentTimeMillis();		
		Integer batchSize = 10000;
		Integer numberOfBatches = (int) (numberOfExecutionImputations / batchSize) + (numberOfExecutionImputations % batchSize == 0 ? 0 : 1);
		LogHelper.logInfo(String.format("taille du lot est de %s. %s lot(s) à traiter",batchSize,numberOfBatches), getClass());
		TransactionResult result = new TransactionResult();
		for(Integer index = 0; index < numberOfBatches; index = index + 1) {
			deriveScopeFunctionsFromModel(executionImputationModel, queryExecutorArguments, batchSize, index*batchSize);
		}
		ExecutionImputationQuerier.refreshMaterializedView();
		LogHelper.logInfo(String.format("Application du modèle terminée. Durée = %s",TimeHelper.formatDuration(System.currentTimeMillis() - t)), getClass());
		return result;
	}
	
	private TransactionResult deriveScopeFunctionsFromModel(ExecutionImputation executionImputationModel,QueryExecutorArguments queryExecutorArguments
			,Integer numberOfExecutionImputations,Integer from) {
		LogHelper.logInfo(String.format("\tChargement de %s imputation(s) à partir l'index %s en cours...",numberOfExecutionImputations,from), getClass());
		queryExecutorArguments.setQueryFromIdentifier(ExecutionImputationQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER).setFirstTupleIndex(from)
		.setNumberOfTuples(numberOfExecutionImputations);
		List<ExecutionImputation> executionImputations = (List<ExecutionImputation>) ExecutionImputationQuerier.getInstance().readWhereFilter(queryExecutorArguments);	
		numberOfExecutionImputations = CollectionHelper.getSize(executionImputations);
		LogHelper.logInfo(String.format("\t%s imputation(s) chargée(s)", numberOfExecutionImputations), getClass());
		if(NumberHelper.isLessThanOrEqualZero(numberOfExecutionImputations))
			return null;
		Integer batchSize = 250;
		Integer numberOfBatches = (int) (numberOfExecutionImputations / batchSize) + (numberOfExecutionImputations % batchSize == 0 ? 0 : 1);		
		LogHelper.logInfo(String.format("\ttaille du lot est de %s. %s lot(s) à traiter",batchSize,numberOfBatches), getClass());				
		
		Executor<SaveScopeFunctionsNativeQueryStringManyBuilderAndExecutor> producer = new Executor<SaveScopeFunctionsNativeQueryStringManyBuilderAndExecutor>()
				.setName("Générateur et exécuteur de requête SQL").setNumberOfRunnablesToBeExecuted(numberOfBatches)
				.setExecutorService(RunnableHelper.instantiateExecutorService(3,5,1l,TimeUnit.MINUTES,null,numberOfBatches,null,null));				
		producer.start();		
		for(Integer index = 0; index < numberOfBatches; index = index + 1) {
			from = index * batchSize;
			Integer to = from + batchSize;
			if(to > executionImputations.size())
				to = executionImputations.size();
			Collection<ExecutionImputation> executionImputationsBatch = executionImputations.subList(from, to);			
			if(CollectionHelper.isEmpty(executionImputationsBatch))
				continue;	
			producer.addRunnables(new SaveScopeFunctionsNativeQueryStringManyBuilderAndExecutor().setExecutionImputationModel(executionImputationModel)
					.setExecutionImputations(executionImputationsBatch));			
		}
		producer.join();
		executionImputations.clear();
		executionImputations = null;
		return null;
	}
	
	@Getter @Setter @Accessors(chain=true)
	public static class SaveScopeFunctionsNativeQueryStringManyBuilderAndExecutor implements Runnable,Serializable {
		private Collection<ExecutionImputation> executionImputations;
		private ExecutionImputation executionImputationModel;
		private TransactionResult result;
			
		@Override
		public void run() {			
			if(CollectionHelper.isEmpty(executionImputations))
				return;
			Collection<Map<String,String>> creatables = new ArrayList<>();
			Collection<Map<String,String>> updatables = new ArrayList<>();
			Collection<Object> deletablesIdentifiers = new ArrayList<>();
			for(ExecutionImputation executionImputation : executionImputations) {
				for(String functionFieldName : ExecutionImputation.FUNCTIONS_FIELDS_NAMES) {
					for(String functionFieldNameType : ExecutionImputation.FUNCTIONS_FIELDS_NAMES_TYPES) {
						if(addIfCreatable(executionImputation, functionFieldName, functionFieldNameType, creatables))
							continue;
						if(addIfUpdatable(executionImputation, functionFieldName, functionFieldNameType, updatables))
							continue;
						if(addIfDeletable(executionImputation, functionFieldName, functionFieldNameType, deletablesIdentifiers))
							continue;							
					}
				}
			}
			/*
			System.out.println(
					"ExecutionImputationBusinessImpl.SaveScopeFunctionsNativeQueryStringManyBuilderAndExecutor.run()");
			System.out.println(creatables);
			System.out.println(updatables);
			System.out.println(deletablesIdentifiers);
			*/
			if(CollectionHelper.isNotEmpty(deletablesIdentifiers)) {
				result.setNumberOfDeletionFromCollection(deletablesIdentifiers);
				NativeQueryStringExecutor.getInstance().execute(new org.cyk.utility.__kernel__.persistence.query.NativeQueryStringExecutor.Arguments()
						.setAction(Action.DELETE).addQueriesStrings(__inject__(NativeQueryStringBuilder.class).buildDeleteManyByIdentifiers(ScopeFunctionExecutionImputation.class, deletablesIdentifiers)));
			}
			
			if(CollectionHelper.isNotEmpty(updatables)) {
				result.setNumberOfUpdate(NumberHelper.getLong(updatables.size()));
				NativeQueryStringExecutor.getInstance().execute(new org.cyk.utility.__kernel__.persistence.query.NativeQueryStringExecutor.Arguments()
						.setAction(Action.UPDATE)
						.addQueriesStrings(__inject__(NativeQueryStringBuilder.class).buildUpdateManyFromMaps(ScopeFunctionExecutionImputation.class, updatables)));
			}
			
			if(CollectionHelper.isNotEmpty(creatables)) {
				result.setNumberOfCreation(NumberHelper.getLong(creatables.size()));
				NativeQueryStringExecutor.getInstance().execute(new org.cyk.utility.__kernel__.persistence.query.NativeQueryStringExecutor.Arguments()
						.setAction(Action.CREATE)
						.addQueriesStrings(__inject__(NativeQueryStringBuilder.class).buildInsertManyFromMaps(ScopeFunctionExecutionImputation.class, creatables)));
			}
		}
		
		private Boolean addIfCreatable(ExecutionImputation executionImputation,String functionFieldName,String type,Collection<Map<String,String>> creatables) {
			String scopeFunctionExecutionImputationIdentifier = (String) FieldHelper.read(executionImputation
					, ExecutionImputation.buildScopeFunctionExecutionImputationIdentifierFieldName(functionFieldName, type));
			if(StringHelper.isNotBlank(scopeFunctionExecutionImputationIdentifier))
				return Boolean.FALSE;
			ExecutionImputationScopeFunction executionImputationScopeFunctionModel = (ExecutionImputationScopeFunction) FieldHelper.read(computeModel(executionImputation), functionFieldName);
			String identifier = ExecutionImputation.FUNCTION_FIELD_NAME_TYPE_HOLDER.equals(type) ? executionImputationScopeFunctionModel.getHolderIdentifier()
					: executionImputationScopeFunctionModel.getAssistantIdentifier();
			if(StringHelper.isNotBlank(identifier)) {
				creatables.add(Map.of(ScopeFunctionExecutionImputation.COLUMN_IDENTIFIER
						,PersistenceHelper.stringifyColumnValue(ScopeFunctionExecutionImputationBusiness.identify(identifier, executionImputation.getIdentifier()))
						,ScopeFunctionExecutionImputation.COLUMN_SCOPE_FUNCTION,PersistenceHelper.stringifyColumnValue(identifier)
						,ScopeFunctionExecutionImputation.COLUMN_EXECUTION_IMPUTATION,PersistenceHelper.stringifyColumnValue(executionImputation.getIdentifier())
						));
					return Boolean.TRUE;
			}
			return Boolean.FALSE;
		}
		
		private Boolean addIfUpdatable(ExecutionImputation executionImputation,String functionFieldName,String type,Collection<Map<String,String>> updatables) {
			String scopeFunctionExecutionImputationIdentifier = (String) FieldHelper.read(executionImputation
					, ExecutionImputation.buildScopeFunctionExecutionImputationIdentifierFieldName(functionFieldName, type));
			if(StringHelper.isBlank(scopeFunctionExecutionImputationIdentifier))
				return Boolean.FALSE;		
			ExecutionImputationScopeFunction executionImputationScopeFunctionModel = (ExecutionImputationScopeFunction) FieldHelper.read(computeModel(executionImputation), functionFieldName);
			String identifier = ExecutionImputation.FUNCTION_FIELD_NAME_TYPE_HOLDER.equals(type) ? executionImputationScopeFunctionModel.getHolderIdentifier()
					: executionImputationScopeFunctionModel.getAssistantIdentifier();
			Boolean overridable = ExecutionImputation.FUNCTION_FIELD_NAME_TYPE_HOLDER.equals(type) ? executionImputationScopeFunctionModel.getHolderOverridable()
					: executionImputationScopeFunctionModel.getAssistantOverridable();
			if(StringHelper.isNotBlank(identifier) && (executionImputationModel == null || Boolean.TRUE.equals(overridable))) {
				if(identifier.equals(FieldHelper.read(executionImputation, ExecutionImputation.buildScopeFunctionIdentifierFieldName(functionFieldName, type))))
					return Boolean.TRUE;
				updatables.add(Map.of(ScopeFunctionExecutionImputation.COLUMN_IDENTIFIER,PersistenceHelper.stringifyColumnValue(scopeFunctionExecutionImputationIdentifier)
						,ScopeFunctionExecutionImputation.COLUMN_SCOPE_FUNCTION,PersistenceHelper.stringifyColumnValue(identifier)
						));
					return Boolean.TRUE;
			}
			return Boolean.FALSE;
		}
		
		private Boolean addIfDeletable(ExecutionImputation executionImputation,String functionFieldName,String type,Collection<Object> deletablesIdentifiers) {
			String scopeFunctionExecutionImputationIdentifier = (String) FieldHelper.read(executionImputation
					, ExecutionImputation.buildScopeFunctionExecutionImputationIdentifierFieldName(functionFieldName, type));
			if(StringHelper.isBlank(scopeFunctionExecutionImputationIdentifier))
				return Boolean.FALSE;
			ExecutionImputationScopeFunction executionImputationScopeFunctionModel = (ExecutionImputationScopeFunction) FieldHelper.read(computeModel(executionImputation), functionFieldName);
			String identifier = ExecutionImputation.FUNCTION_FIELD_NAME_TYPE_HOLDER.equals(type) ? executionImputationScopeFunctionModel.getHolderIdentifier()
					: executionImputationScopeFunctionModel.getAssistantIdentifier();
			Boolean overridable = ExecutionImputation.FUNCTION_FIELD_NAME_TYPE_HOLDER.equals(type) ? executionImputationScopeFunctionModel.getHolderOverridable()
					: executionImputationScopeFunctionModel.getAssistantOverridable();
			if(StringHelper.isBlank(identifier) && (executionImputationModel == null || Boolean.TRUE.equals(overridable))) {
				deletablesIdentifiers.add(scopeFunctionExecutionImputationIdentifier);
				return Boolean.TRUE;
			}
			return Boolean.FALSE;
		}
		
		private ExecutionImputation computeModel(ExecutionImputation executionImputation) {
			if(executionImputationModel == null)
				return executionImputation;
			return executionImputationModel;
		}
	}
}