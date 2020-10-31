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
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionExecutionImputationQuerier;
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
		TransactionResult result = new TransactionResult().setTupleName("affectation");
		List<ScopeFunctionExecutionImputation> savables = new ArrayList<>();
		List<ScopeFunctionExecutionImputation> deletables = new ArrayList<>();
		
		LogHelper.logInfo(String.format("Chargement des affectations existantes des imputations en cours..."), getClass());
		Long t = System.currentTimeMillis();
		Collection<ScopeFunctionExecutionImputation> database = ScopeFunctionExecutionImputationQuerier.getInstance().readByExecutionImputations(executionImputations);
		LogHelper.logInfo(String.format("Chargement des affectations existantes des imputations en %s",TimeHelper.formatDuration(System.currentTimeMillis() - t)), getClass());
		
		LogHelper.logInfo(String.format("Collecte des instances a créer et a supprimer en cours..."), getClass());
		t = System.currentTimeMillis();
		for(ExecutionImputation executionImputation : executionImputations)
			process(executionImputation,ScopeFunctionExecutionImputation.filterBy(executionImputation, database),savables,deletables);
		LogHelper.logInfo(String.format("Collecte des instances a créer et a supprimer en %s",TimeHelper.formatDuration(System.currentTimeMillis() - t)), getClass());
		
		LogHelper.logInfo(String.format("Création et suppression en cours..."), getClass());
		t = System.currentTimeMillis();
		result.setFromSavables(savables).setNumberOfDeletionFromCollection(deletables);				
		if(CollectionHelper.isNotEmpty(savables))
			;//__inject__(ScopeFunctionExecutionImputationBusiness.class).saveMany(savables);		
		if(CollectionHelper.isNotEmpty(deletables)) {
			List<List<ScopeFunctionExecutionImputation>> batches = CollectionHelper.getBatches(deletables, 999);
			for(List<ScopeFunctionExecutionImputation> batch : batches)
				;//__inject__(ScopeFunctionExecutionImputationBusiness.class).deleteMany(batch);
		}
		LogHelper.logInfo(String.format("Création et suppression en %s",TimeHelper.formatDuration(System.currentTimeMillis() - t)), getClass());
		
		result.log(getClass());
		return result;
	}
	
	private static void process(ExecutionImputation executionImputation
			,Collection<ScopeFunctionExecutionImputation> database,Collection<ScopeFunctionExecutionImputation> savables,Collection<ScopeFunctionExecutionImputation> deletables) {
		if(executionImputation == null)
			return;
		process(executionImputation, executionImputation.computeScopeFunctions(),database,savables,deletables);
	}
	
	private static void process(ExecutionImputation executionImputation,Collection<ScopeFunction> scopeFunctions
			,Collection<ScopeFunctionExecutionImputation> database,Collection<ScopeFunctionExecutionImputation> savables,Collection<ScopeFunctionExecutionImputation> deletables) {
		if(executionImputation == null)
			return;
		Collection<ScopeFunctionExecutionImputation> undeletables = null;
		if(CollectionHelper.isEmpty(scopeFunctions)) {
			
		}else {
			ScopeFunctionExecutionImputation scopeFunctionExecutionImputation;
			for(ScopeFunction scopeFunction : scopeFunctions) {
				//if function identifier exists then go to next
				if((scopeFunctionExecutionImputation = ScopeFunctionExecutionImputation.find(scopeFunction, executionImputation, database)) != null) {
					if(undeletables == null)
						undeletables = new ArrayList<>();
					undeletables.add(scopeFunctionExecutionImputation);
					continue;
				}
				//find by function code
				scopeFunctionExecutionImputation = ScopeFunctionExecutionImputation.find(scopeFunction.getFunction().getCode(), database);
				if(scopeFunctionExecutionImputation == null) {
					//new instance to create
					scopeFunctionExecutionImputation = new ScopeFunctionExecutionImputation().setScopeFunction(scopeFunction).setExecutionImputation(executionImputation);
				}else {
					//existing instance where function to be updated
					scopeFunctionExecutionImputation.setScopeFunction(scopeFunction);
				}					
				savables.add(scopeFunctionExecutionImputation);
				if(undeletables == null)
					undeletables = new ArrayList<>();
				undeletables.add(scopeFunctionExecutionImputation);
			}
		}
		if(CollectionHelper.isNotEmpty(database)) {
			for(ScopeFunctionExecutionImputation index : database) {
				if(CollectionHelper.contains(database, index) && !CollectionHelper.contains(undeletables, index))
					deletables.add(index);
			}
		}
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

	@Override
	public TransactionResult deriveScopeFunctionsFromModel(ExecutionImputation executionImputationModel,Filter filter) {
		ThrowableHelper.throwIllegalArgumentExceptionIfNull("executionImputationModel", executionImputationModel);
		ThrowableHelper.throwIllegalArgumentExceptionIfNull("filter", filter);
		/*
		if(executionImputationModel.getCreditManagerHolder() == null && executionImputationModel.getAuthorizingOfficerHolder() == null 
				&& executionImputationModel.getFinancialControllerHolder() == null && executionImputationModel.getAccountingHolder() == null)
			throw new RuntimeException("Valeur modèle obligatoire");
		*/
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
		
		Executor<DeriveScopeFunctionsFromModelNativeQueryStringManyBuilder> producer = new Executor<DeriveScopeFunctionsFromModelNativeQueryStringManyBuilder>()
				.setName("Génération de requête SQL").setNumberOfRunnablesToBeExecuted(numberOfBatches)
				.setExecutorService(RunnableHelper.instantiateExecutorService(3,5,1l,TimeUnit.MINUTES,null,numberOfBatches,null,null))
				.setListener(new Executor.Listener.AbstractImpl<DeriveScopeFunctionsFromModelNativeQueryStringManyBuilder>() {
					@Override
					public void listenCompletion(Executor<DeriveScopeFunctionsFromModelNativeQueryStringManyBuilder> executor, DeriveScopeFunctionsFromModelNativeQueryStringManyBuilder runnable) {
						super.listenCompletion(executor, runnable);
						//System.out.println("DEL : "+((DeriveScopeFunctionsFromModelNativeQueryStringManyBuilder)runnable).getDelete());
						if(StringHelper.isNotBlank(runnable.getDelete()))
							NativeQueryStringExecutor.getInstance().execute(new org.cyk.utility.__kernel__.persistence.query.NativeQueryStringExecutor.Arguments()
									.setAction(Action.DELETE).addQueriesStrings(runnable.getDelete()));
						if(StringHelper.isNotBlank(runnable.getUpdate()))
							NativeQueryStringExecutor.getInstance().execute(new org.cyk.utility.__kernel__.persistence.query.NativeQueryStringExecutor.Arguments()
									.setAction(Action.UPDATE).addQueriesStrings(runnable.getUpdate()));
						if(StringHelper.isNotBlank(runnable.getCreate()))
							NativeQueryStringExecutor.getInstance().execute(new org.cyk.utility.__kernel__.persistence.query.NativeQueryStringExecutor.Arguments()
									.setAction(Action.CREATE).addQueriesStrings(runnable.getCreate()));
					}
				});				
		//producer.start();
		
		for(Integer index = 0; index < numberOfBatches; index = index + 1) {
			Integer from = index * batchSize;
			Integer to = from + batchSize;
			if(to > executionImputations.size())
				to = executionImputations.size();
			Collection<ExecutionImputation> executionImputationsBatch = executionImputations.subList(from, to);			
			if(CollectionHelper.isEmpty(executionImputationsBatch))
				continue;	
			producer.addRunnables(new DeriveScopeFunctionsFromModelNativeQueryStringManyBuilder().setExecutionImputationModel(executionImputationModel)
					.setExecutionImputations(executionImputationsBatch));			
			//TransactionResult transactionResult = saveScopeFunctions(executionImputations);
			//result.add(transactionResult);
			//break;
		}
		producer.run();
		//producer.join();
		ExecutionImputationQuerier.refreshMaterializedView();
		result.log(getClass());
		return result;
	}
	
	@Getter @Setter @Accessors(chain=true)
	public static class DeriveScopeFunctionsFromModelNativeQueryStringManyBuilder implements Runnable,Serializable {
		private Collection<ExecutionImputation> executionImputations;
		private ExecutionImputation executionImputationModel;
		private String create,update,delete;
			
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
			if(CollectionHelper.isNotEmpty(creatables))
				create = __inject__(NativeQueryStringBuilder.class).buildInsertManyFromMaps(ScopeFunctionExecutionImputation.class, creatables);
			if(CollectionHelper.isNotEmpty(updatables))
				update = __inject__(NativeQueryStringBuilder.class).buildUpdateManyFromMaps(ScopeFunctionExecutionImputation.class, updatables);
			if(CollectionHelper.isNotEmpty(deletablesIdentifiers))
				delete = __inject__(NativeQueryStringBuilder.class).buildDeleteManyByIdentifiers(ScopeFunctionExecutionImputation.class, deletablesIdentifiers);			
		}
		
		private Boolean addIfCreatable(ExecutionImputation executionImputation,String functionFieldName,String type,Collection<Map<String,String>> creatables) {
			String scopeFunctionExecutionImputationIdentifier = (String) FieldHelper.read(executionImputation
					, ExecutionImputation.buildScopeFunctionExecutionImputationIdentifierFieldName(functionFieldName, type));
			if(StringHelper.isNotBlank(scopeFunctionExecutionImputationIdentifier))
				return Boolean.FALSE;
			ExecutionImputationScopeFunction executionImputationScopeFunctionModel = (ExecutionImputationScopeFunction) FieldHelper.read(executionImputationModel, functionFieldName);
			if(executionImputationScopeFunctionModel != null && StringHelper.isNotBlank(executionImputationScopeFunctionModel.getHolderIdentifier())) {
				creatables.add(Map.of(ScopeFunctionExecutionImputation.COLUMN_IDENTIFIER
						,"'"+ScopeFunctionExecutionImputationBusiness.identify(executionImputationScopeFunctionModel.getHolderIdentifier(), executionImputation.getIdentifier())+"'"
						,ScopeFunctionExecutionImputation.COLUMN_SCOPE_FUNCTION,"'"+executionImputationScopeFunctionModel.getHolderIdentifier()+"'"
						,ScopeFunctionExecutionImputation.COLUMN_EXECUTION_IMPUTATION,"'"+executionImputation.getIdentifier()+"'"
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
			ExecutionImputationScopeFunction executionImputationScopeFunctionModel = (ExecutionImputationScopeFunction) FieldHelper.read(executionImputationModel, functionFieldName);
			if(executionImputationScopeFunctionModel != null && StringHelper.isNotBlank(executionImputationScopeFunctionModel.getHolderIdentifier())
					&& Boolean.TRUE.equals(executionImputationScopeFunctionModel.getHolderOverridable())) {
				updatables.add(Map.of(ScopeFunctionExecutionImputation.COLUMN_IDENTIFIER
						,"'"+scopeFunctionExecutionImputationIdentifier+"'"
						,ScopeFunctionExecutionImputation.COLUMN_SCOPE_FUNCTION,"'"+executionImputationScopeFunctionModel.getHolderIdentifier()+"'"
						,ScopeFunctionExecutionImputation.COLUMN_EXECUTION_IMPUTATION,"'"+executionImputation.getIdentifier()+"'"
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
			ExecutionImputationScopeFunction executionImputationScopeFunctionModel = (ExecutionImputationScopeFunction) FieldHelper.read(executionImputationModel, functionFieldName);
			if(executionImputationScopeFunctionModel != null && StringHelper.isBlank(executionImputationScopeFunctionModel.getHolderIdentifier()) 
						&& Boolean.TRUE.equals(executionImputationScopeFunctionModel.getHolderOverridable())) {
					deletablesIdentifiers.add(scopeFunctionExecutionImputationIdentifier);
					return Boolean.TRUE;
			}
			return Boolean.FALSE;
		}
	}
}