package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.log.LogHelper;
import org.cyk.utility.__kernel__.number.NumberHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.throwable.ThrowableHelper;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;
import org.cyk.utility.server.business.BusinessServiceProvider;

import ci.gouv.dgbf.system.actor.server.business.api.ScopeFunctionBusiness;
import ci.gouv.dgbf.system.actor.server.business.api.ScopeFunctionExecutionImputationBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ScopeFunctionExecutionImputationPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ExecutionImputationQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionExecutionImputationQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunctionExecutionImputation;

@ApplicationScoped
public class ScopeFunctionExecutionImputationBusinessImpl extends AbstractBusinessEntityImpl<ScopeFunctionExecutionImputation, ScopeFunctionExecutionImputationPersistence> implements ScopeFunctionExecutionImputationBusiness,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public void deriveFromExecutionImputations(Collection<ExecutionImputation> executionImputations) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("executionImputations", executionImputations);
		LogHelper.logInfo(String.format("Création des assignations à partir de %s imputation(s) en cours",executionImputations.size()), getClass());
		Long t0 = System.currentTimeMillis();
		Long count0 = __persistence__.count();
		/**/
		//deriveFromExecutionImputations(executionImputations, scopeFunctions, existingScopeFunctionExecutionImputations);
		/**/
		Long duration = System.currentTimeMillis() - t0;
		Long numberOfElements = __persistence__.count() - count0;
		LogHelper.logInfo(String.format("%s assignation(s) crée(s) en %s", numberOfElements,duration), getClass());
	}

	@Override
	public void deriveAll() {
		System.gc();
		Long t0 = System.currentTimeMillis();
		Long count0 = __persistence__.count();
		LogHelper.logInfo(String.format("Création de toutes les assignations en cours"), getClass());
		//load scopeFunctions
		Long numberOfScopeFunctions = ScopeFunctionQuerier.getInstance().count();
		if(NumberHelper.isEqualToZero(numberOfScopeFunctions))
			return;
		LogHelper.logInfo(String.format("Chargement de %s poste(s) en cours...",numberOfScopeFunctions), getClass());
		List<ScopeFunction> scopeFunctions = (List<ScopeFunction>) ScopeFunctionQuerier.getInstance().readAllWithReferencesOnly();
		LogHelper.logInfo(String.format("%s poste(s) chargé",scopeFunctions.size()), getClass());
		//load executionImputations
		Long numberOfExecutionImputations = ExecutionImputationQuerier.getInstance().countWhereScopeFunctionDoesNotExistWithReferencesOnly();
		if(NumberHelper.isEqualToZero(numberOfExecutionImputations))
			return;
		LogHelper.logInfo(String.format("Chargement de %s imputation(s) en cours...",numberOfExecutionImputations), getClass());
		List<ExecutionImputation> executionImputations = (List<ExecutionImputation>) ExecutionImputationQuerier.getInstance().readWhereScopeFunctionDoesNotExistWithReferencesOnly(new QueryExecutorArguments());
		LogHelper.logInfo(String.format("%s imputation(s) chargée(s)",numberOfExecutionImputations), getClass());
		//processing
		Integer batchSize = 10000;
		Integer numberOfBatches = (int) (numberOfExecutionImputations / batchSize) + (numberOfExecutionImputations % batchSize == 0 ? 0 : 1);
		LogHelper.logInfo(String.format("taille du lot est de %s. %s lot(s) à traiter",batchSize,numberOfBatches), getClass());		
		Collection<ScopeFunctionExecutionImputation> existingScopeFunctionExecutionImputations = ScopeFunctionExecutionImputationQuerier.getInstance().readAllWithReferencesOnly();
		LogHelper.logInfo(String.format("%s assignation(s) existante(s) chargée(s) en mémoire",CollectionHelper.getSize(existingScopeFunctionExecutionImputations)), getClass());
		ScopeFunctionBusiness scopeFunctionBusiness = __inject__(ScopeFunctionBusiness.class);
		for(Integer index = 0; index < numberOfBatches; index = index + 1) {
			Integer from = index * batchSize;
			Integer to = from + batchSize;
			if(to > executionImputations.size())
				to = executionImputations.size();
			LogHelper.logInfo(String.format("Lecture du lot %s à partir de l'index %s(%s).",index+1,from,executionImputations.size()), getClass());
			Collection<ExecutionImputation> executionImputationsBatch = executionImputations.subList(from, to);
			LogHelper.logInfo(String.format("%s imputation(s) lue(s)",CollectionHelper.getSize(executionImputationsBatch)), getClass());
			if(CollectionHelper.isEmpty(executionImputationsBatch))
				continue;
			deriveFromExecutionImputations(scopeFunctionBusiness,executionImputationsBatch, scopeFunctions,existingScopeFunctionExecutionImputations);
			//executionImputationsBatch.clear();
			//executionImputationsBatch = null;
		}
		/*
		Collection<Runnable> runnables = new ArrayList<>();
		for(Integer index = 0; index < numberOfBatches; index = index + 1) {
			Integer from = index * batchSize;
			LogHelper.logInfo(String.format("chargement en mémoire de %s imputation(s) à partir de l'index %s",batchSize,from), getClass());
			final Collection<ExecutionImputation> executionImputations = ExecutionImputationQuerier.getInstance().readAllWithReferencesOnly(
					new QueryExecutorArguments().setFirstTupleIndex(from).setNumberOfTuples(batchSize));
			LogHelper.logInfo(String.format("%s imputation(s) trouvée(s)",executionImputations.size()), getClass());
			if(CollectionHelper.isEmpty(executionImputations))
				continue;
			runnables.add(new Runnable() {			
				@Override
				public void run() {
					deriveFromExecutionImputations(scopeFunctionBusiness,executionImputations, scopeFunctions,existingScopeFunctionExecutionImputations);
					executionImputations.clear();
				}
			});
		}
		RunnableHelper.run(runnables, "create assignation from execution imputations");
		*/
		Long duration = System.currentTimeMillis() - t0;
		Long numberOfElements = __persistence__.count() - count0;
		LogHelper.logInfo(String.format("%s assignation(s) crée(s) en %s", numberOfElements,duration), getClass());
		if(scopeFunctions != null) {
			scopeFunctions.clear();
			scopeFunctions = null;
		}
		if(executionImputations != null) {
			executionImputations.clear();
			executionImputations = null;
		}
		if(existingScopeFunctionExecutionImputations != null) {
			existingScopeFunctionExecutionImputations.clear();
			existingScopeFunctionExecutionImputations = null;
		}
		System.gc();
	}
	
	private void deriveFromExecutionImputations(ScopeFunctionBusiness scopeFunctionBusiness,Collection<ExecutionImputation> executionImputations,Collection<ScopeFunction> scopeFunctions,Collection<ScopeFunctionExecutionImputation> existingScopeFunctionExecutionImputations) {
		System.gc();
		if(CollectionHelper.isEmpty(executionImputations))
			return;		
		Long t0 = System.currentTimeMillis();
		Long count0 = __persistence__.count();
		LogHelper.logInfo(String.format("\tinstantiation de %s probable assignation(s) en cours",executionImputations.size()*4), getClass());
		if(CollectionHelper.isEmpty(executionImputations))
			return;
		
		List<ScopeFunctionExecutionImputation> scopeFunctionExecutionImputations = new ArrayList<>();
		/*
		Collection<Runnable> runnables = new ArrayList<>();
		for(ExecutionImputation executionImputation : executionImputations) {
			runnables.add(new Runnable() {			
				@Override
				public void run() {
					add(scopeFunctionBusiness.computeCode(executionImputation.getAdministrativeUnitCodeName(), Function.CODE_CREDIT_MANAGER_HOLDER)
							, executionImputation, scopeFunctionExecutionImputations, scopeFunctions,existingScopeFunctionExecutionImputations);
					
					add(scopeFunctionBusiness.computeCode(executionImputation.getBudgetSpecializationUnitCodeName(), Function.CODE_AUTHORIZING_OFFICER_HOLDER)
							, executionImputation, scopeFunctionExecutionImputations, scopeFunctions,existingScopeFunctionExecutionImputations);
					
					add(scopeFunctionBusiness.computeCode(executionImputation.getSectionCodeName()
							, Function.CODE_FINANCIAL_CONTROLLER_HOLDER), executionImputation, scopeFunctionExecutionImputations, scopeFunctions,existingScopeFunctionExecutionImputations);
					
					add(scopeFunctionBusiness.computeCode(executionImputation.getSectionCodeName()
							, Function.CODE_ACCOUNTING_HOLDER), executionImputation, scopeFunctionExecutionImputations, scopeFunctions,existingScopeFunctionExecutionImputations);			
				}
			});			
		}
		RunnableHelper.run(runnables, "add assignation");
		*/
		
		for(ExecutionImputation executionImputation : executionImputations) {
			add(scopeFunctionBusiness.computeCode(executionImputation.getAdministrativeUnitCodeName(), Function.CODE_CREDIT_MANAGER_HOLDER)
					, executionImputation, scopeFunctionExecutionImputations, scopeFunctions,existingScopeFunctionExecutionImputations);
			
			add(scopeFunctionBusiness.computeCode(executionImputation.getBudgetSpecializationUnitCodeName(), Function.CODE_AUTHORIZING_OFFICER_HOLDER)
					, executionImputation, scopeFunctionExecutionImputations, scopeFunctions,existingScopeFunctionExecutionImputations);
			
			add(scopeFunctionBusiness.computeCode(executionImputation.getSectionCodeName()
					, Function.CODE_FINANCIAL_CONTROLLER_HOLDER), executionImputation, scopeFunctionExecutionImputations, scopeFunctions,existingScopeFunctionExecutionImputations);
			
			add(scopeFunctionBusiness.computeCode(executionImputation.getSectionCodeName()
					, Function.CODE_ACCOUNTING_HOLDER), executionImputation, scopeFunctionExecutionImputations, scopeFunctions,existingScopeFunctionExecutionImputations);
		}
				
		LogHelper.logInfo(String.format("\t%s assignation(s) instantiée(s)",CollectionHelper.getSize(scopeFunctionExecutionImputations)), getClass());
		if(CollectionHelper.isNotEmpty(scopeFunctionExecutionImputations)) {
			Integer batchSize = 10000;
			LogHelper.logInfo("\tCréation par lot de "+batchSize+" en cours",getClass());
			createByBatch(scopeFunctionExecutionImputations, batchSize);
		}
		Long duration = System.currentTimeMillis() - t0;
		Long numberOfElements = __persistence__.count() - count0;
		LogHelper.logInfo(String.format("\t%s assignation(s) crée(s) en %s", numberOfElements,duration), getClass());
		System.gc();
	}
	
	@Override
	public BusinessServiceProvider<ScopeFunctionExecutionImputation> createMany(Collection<ScopeFunctionExecutionImputation> scopeFunctionExecutionImputations) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("scopeFunctionExecutionImputations", scopeFunctionExecutionImputations);
		__persistence__.createMany(scopeFunctionExecutionImputations);
		return this;
	}
	
	private static void add(String scopeFunctionCode,ExecutionImputation executionImputation,Collection<ScopeFunctionExecutionImputation> scopeFunctionExecutionImputations
			,Collection<ScopeFunction> scopeFunctions,Collection<ScopeFunctionExecutionImputation> existingScopeFunctionExecutionImputations) {
		ScopeFunction scopeFunction = getScopeFunctionByCode(scopeFunctionCode,scopeFunctions);
		if(scopeFunction == null) {
			LogHelper.logWarning(String.format("poste %s inexistant", scopeFunctionCode), ScopeFunctionExecutionImputationBusinessImpl.class);
			return;
		}
		//if(Boolean.TRUE.equals(ScopeFunctionExecutionImputationQuerier.getInstance().exist(scopeFunction, executionImputation)))
		//	return;
		if(Boolean.TRUE.equals(isExist(scopeFunction, executionImputation, existingScopeFunctionExecutionImputations)))
			return;
		scopeFunctionExecutionImputations.add(new ScopeFunctionExecutionImputation().setScopeFunction(scopeFunction).setExecutionImputation(executionImputation));
	}
	
	private static ScopeFunction getScopeFunctionByCode(String scopeFunctionCode,Collection<ScopeFunction> scopeFunctions) {
		for(ScopeFunction scopeFunction : scopeFunctions)
			if(scopeFunction.getCode().equals(scopeFunctionCode))
				return scopeFunction;
		return null;
	}
	
	private static Boolean isExist(ScopeFunction scopeFunction,ExecutionImputation executionImputation,Collection<ScopeFunctionExecutionImputation> scopeFunctionExecutionImputations) {
		if(scopeFunction == null || executionImputation == null || CollectionHelper.isEmpty(scopeFunctionExecutionImputations))
			return Boolean.FALSE;
		for(ScopeFunctionExecutionImputation scopeFunctionExecutionImputation : scopeFunctionExecutionImputations)
			if(scopeFunctionExecutionImputation.getScopeFunctionIdentifier().equals(scopeFunction.getIdentifier()) 
					&& scopeFunctionExecutionImputation.getExecutionImputationIdentifier().equals(executionImputation.getIdentifier())) {
				//System.out.println("ScopeFunctionExecutionImputationBusinessImpl.isExist() : "+scopeFunctionExecutionImputation.getIdentifier()
				//		+" - "+scopeFunctionExecutionImputation.getScopeFunctionIdentifier()+" - "
				//		+scopeFunctionExecutionImputation.getExecutionImputationIdentifier());
				return Boolean.TRUE;
			}
		return Boolean.FALSE;
	}
}