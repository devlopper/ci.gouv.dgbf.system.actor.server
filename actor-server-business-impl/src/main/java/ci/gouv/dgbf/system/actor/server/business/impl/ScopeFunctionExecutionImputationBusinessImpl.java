package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.TimeUnit;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.__kernel__.business.NativeQueryStringExecutor;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.enumeration.Action;
import org.cyk.utility.__kernel__.log.LogHelper;
import org.cyk.utility.__kernel__.number.NumberHelper;
import org.cyk.utility.__kernel__.object.marker.IdentifiableSystem;
import org.cyk.utility.__kernel__.persistence.query.NativeQueryStringBuilder;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.runnable.Executor;
import org.cyk.utility.__kernel__.runnable.RunnableHelper;
import org.cyk.utility.__kernel__.throwable.ThrowableHelper;
import org.cyk.utility.__kernel__.time.TimeHelper;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.ScopeFunctionExecutionImputationBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ScopeFunctionExecutionImputationPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ExecutionImputationQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunctionExecutionImputation;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

@ApplicationScoped
public class ScopeFunctionExecutionImputationBusinessImpl extends AbstractBusinessEntityImpl<ScopeFunctionExecutionImputation, ScopeFunctionExecutionImputationPersistence> implements ScopeFunctionExecutionImputationBusiness,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public void deriveFromExecutionImputations(Collection<ExecutionImputation> executionImputations) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("executionImputations", executionImputations);
		LogHelper.logInfo(String.format("Création des assignations à partir de %s imputation(s) en cours",executionImputations.size()), getClass());
		Long t0 = System.currentTimeMillis();
		ExecutionImputationQuerier.refreshMaterializedView();
		Long count0 = __persistence__.count();
		/**/
		//deriveFromExecutionImputations(executionImputations, scopeFunctions, existingScopeFunctionExecutionImputations);
		/**/
		Long duration = System.currentTimeMillis() - t0;
		Long numberOfElements = __persistence__.count() - count0;
		LogHelper.logInfo(String.format("%s assignation(s) crée(s) en %s", numberOfElements,duration), getClass());
		ExecutionImputationQuerier.refreshMaterializedView();
	}

	@Override
	public void deriveAll() {
		System.gc();
		Long t0 = System.currentTimeMillis();
		ExecutionImputationQuerier.refreshMaterializedView();
		Long count0 = __persistence__.count();
		LogHelper.logInfo(String.format("Création de toutes les assignations en cours"), getClass());
		
		//load executionImputations
		Long numberOfExecutionImputations = ExecutionImputationQuerier.getInstance().countWhereScopeFunctionDoesNotExistWithReferencesOnly();
		LogHelper.logInfo(String.format("%s imputation(s) non complet(s)",numberOfExecutionImputations), getClass());
		if(NumberHelper.isEqualToZero(numberOfExecutionImputations))
			return;
		
		//load scopeFunctions
		Long numberOfScopeFunctions = ScopeFunctionQuerier.getInstance().count();
		LogHelper.logInfo(String.format("%s poste(s)",numberOfScopeFunctions), getClass());
		if(NumberHelper.isEqualToZero(numberOfScopeFunctions))
			return;
		LogHelper.logInfo(String.format("Chargement des postes en mémoire..."), getClass());
		List<ScopeFunction> scopeFunctions = (List<ScopeFunction>) ScopeFunctionQuerier.getInstance().readAllWithReferencesOnly();
		LogHelper.logInfo(String.format("%s poste(s) chargé(s)",scopeFunctions.size()), getClass());
		
		LogHelper.logInfo(String.format("Chargement des imputations à traiter en mémoire...",numberOfExecutionImputations), getClass());
		List<ExecutionImputation> executionImputations = (List<ExecutionImputation>) ExecutionImputationQuerier.getInstance().readWhereScopeFunctionDoesNotExistWithReferencesOnly(new QueryExecutorArguments());
		LogHelper.logInfo(String.format("%s imputation(s) chargée(s)",numberOfExecutionImputations), getClass());
				
		Integer batchSize = 100;
		Integer numberOfBatches = (int) (numberOfExecutionImputations / batchSize) + (numberOfExecutionImputations % batchSize == 0 ? 0 : 1);
		LogHelper.logInfo(String.format("Taille du lot est de %s. %s lot(s) à traiter",batchSize,numberOfBatches), getClass());
		
		Executor<DeriveAllNativeQueryStringInsertManyExecutor> consumer = new Executor<DeriveAllNativeQueryStringInsertManyExecutor>().setName("Exécution de requête SQL").setNumberOfRunnablesToBeExecuted(numberOfBatches)
				.setExecutorService(RunnableHelper.instantiateExecutorService(10,20,numberOfBatches * 45l / 4,TimeUnit.SECONDS,null
					,numberOfBatches,null,null));
		consumer.start();
		
		Executor<DeriveAllNativeQueryStringInsertManyBuilder> producer = new Executor<DeriveAllNativeQueryStringInsertManyBuilder>().setName("Génération de requête SQL").setNumberOfRunnablesToBeExecuted(numberOfBatches)
				.setExecutorService(RunnableHelper.instantiateExecutorService(4,100,1l,TimeUnit.MINUTES,null,numberOfBatches,null,null))
				.setListener(new Executor.Listener.AbstractImpl<DeriveAllNativeQueryStringInsertManyBuilder>() {
					@Override
					public void listenCompletion(Executor<DeriveAllNativeQueryStringInsertManyBuilder> producerConsumer, DeriveAllNativeQueryStringInsertManyBuilder runnable) {
						super.listenCompletion(producerConsumer, runnable);
						consumer.addRunnables(new DeriveAllNativeQueryStringInsertManyExecutor().setQueryString(runnable.getResult()));	
					}
				});				
		producer.start();
									
		for(Integer index = 0; index < numberOfBatches; index = index + 1) {			
			Integer from = index * batchSize;
			Integer to = from + batchSize;
			if(to > executionImputations.size())
				to = executionImputations.size();
			Collection<ExecutionImputation> executionImputationsBatch = executionImputations.subList(from, to);
			if(CollectionHelper.isEmpty(executionImputationsBatch))
				continue;
			producer.addRunnables(new DeriveAllNativeQueryStringInsertManyBuilder().setExecutionImputations(executionImputationsBatch).setScopeFunctions(scopeFunctions));
		}
		producer.join();		
		consumer.join();
		
		Long duration = System.currentTimeMillis() - t0;
		Long numberOfElements = __persistence__.count() - count0;
		LogHelper.logInfo(String.format("%s affectation(s) crée(s) en %s", numberOfElements,TimeHelper.formatDuration(duration)), getClass());
		if(scopeFunctions != null) {
			scopeFunctions.clear();
			scopeFunctions = null;
		}
		if(executionImputations != null) {
			executionImputations.clear();
			executionImputations = null;
		}
		ExecutionImputationQuerier.refreshMaterializedView();
		System.gc();
	}
	
	private static String getNativeQueryStringInsertMany(Collection<ExecutionImputation> executionImputations,Collection<ScopeFunction> scopeFunctions) {
		if(CollectionHelper.isEmpty(executionImputations))
			return null;		
		List<ScopeFunctionExecutionImputation> scopeFunctionExecutionImputations = new ArrayList<>();
		for(ExecutionImputation executionImputation : executionImputations) {
			add(executionImputation.getAdministrativeUnitCodeName(), Function.CODE_CREDIT_MANAGER_HOLDER, executionImputation, scopeFunctions, scopeFunctionExecutionImputations);			
			add(executionImputation.getBudgetSpecializationUnitCodeName(), Function.CODE_AUTHORIZING_OFFICER_HOLDER, executionImputation, scopeFunctions, scopeFunctionExecutionImputations);		
			add(executionImputation.getSectionCodeName(), Function.CODE_FINANCIAL_CONTROLLER_HOLDER, executionImputation, scopeFunctions, scopeFunctionExecutionImputations);		
			add(executionImputation.getSectionCodeName(), Function.CODE_ACCOUNTING_HOLDER, executionImputation, scopeFunctions, scopeFunctionExecutionImputations);
		}
		if(CollectionHelper.isEmpty(scopeFunctionExecutionImputations))
			return null;
		IdentifiableSystem.setManyIfNull(scopeFunctionExecutionImputations);
		return __inject__(NativeQueryStringBuilder.class).buildInsertMany(ScopeFunctionExecutionImputation.class, scopeFunctionExecutionImputations);
	}
	
	private static void add(String scopeCode,String functionCode,ExecutionImputation executionImputation
			,Collection<ScopeFunction> scopeFunctions,Collection<ScopeFunctionExecutionImputation> scopeFunctionExecutionImputations) {
		ScopeFunction scopeFunction = CollectionHelper.isEmpty(scopeFunctions) ? null : findScopeFunction(scopeCode,functionCode,scopeFunctions);
		if(scopeFunction == null) {
			LogHelper.logWarning(String.format("poste (%s,%s) inexistant", functionCode,scopeCode), ScopeFunctionExecutionImputationBusinessImpl.class);
			return;
		}
		if(Boolean.TRUE.equals(executionImputation.hasScopeFunction(scopeFunction))) {
			return;
		}
		scopeFunctionExecutionImputations.add(new ScopeFunctionExecutionImputation().setScopeFunction(scopeFunction).setExecutionImputation(executionImputation));
	}

	public static ScopeFunction findScopeFunction(String scopeCode,String functionCode,Collection<ScopeFunction> scopeFunctions) {
		for(ScopeFunction scopeFunction : scopeFunctions)
			if(scopeFunction.getScopeAsString().equals(scopeCode) && scopeFunction.getFunctionAsString().equals(functionCode))
				return scopeFunction;
		return null;
	}
	
	/**/

	@Getter @Setter @Accessors(chain=true)
	public static class DeriveAllNativeQueryStringInsertManyBuilder implements Runnable,Serializable {
		private Collection<ScopeFunction> scopeFunctions;
		private Collection<ExecutionImputation> executionImputations;
		private String result;
			
		@Override
		public void run() {
			result = getNativeQueryStringInsertMany(executionImputations, scopeFunctions);		
		}		
	}
	
	@Getter @Setter @Accessors(chain=true)
	public static class DeriveAllNativeQueryStringInsertManyExecutor implements Runnable,Serializable {
		private String queryString;
		@Override
		public void run() {
			NativeQueryStringExecutor.getInstance().execute(new org.cyk.utility.__kernel__.persistence.query.NativeQueryStringExecutor.Arguments()
					.setAction(Action.CREATE).addQueriesStrings(queryString));
		}
	}
}