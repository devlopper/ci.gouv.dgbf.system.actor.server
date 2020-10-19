package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import javax.enterprise.context.ApplicationScoped;
import javax.transaction.Transactional;

import org.cyk.utility.__kernel__.business.TransactionResult;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.log.LogHelper;
import org.cyk.utility.__kernel__.throwable.ThrowableHelper;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.ExecutionImputationBusiness;
import ci.gouv.dgbf.system.actor.server.business.api.ScopeFunctionExecutionImputationBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ExecutionImputationPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionExecutionImputationQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunctionExecutionImputation;

@ApplicationScoped
public class ExecutionImputationBusinessImpl extends AbstractBusinessEntityImpl<ExecutionImputation, ExecutionImputationPersistence> implements ExecutionImputationBusiness,Serializable {
	private static final long serialVersionUID = 1L;

	@Override @Transactional
	public TransactionResult saveScopeFunctions(Collection<ExecutionImputation> executionImputations) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("execution imputations", executionImputations);
		LogHelper.logInfo(String.format("Enregistrement des assignations de "+executionImputations.size()+" imputation(s)"), getClass());
		TransactionResult result = new TransactionResult().setTupleName("assignation");
		Collection<ScopeFunctionExecutionImputation> savables = new ArrayList<>();
		Collection<ScopeFunctionExecutionImputation> deletables = new ArrayList<>();
		Collection<ScopeFunctionExecutionImputation> database = ScopeFunctionExecutionImputationQuerier.getInstance()
				.readByExecutionImputations(executionImputations);
		for(ExecutionImputation executionImputation : executionImputations)
			process(executionImputation,ScopeFunctionExecutionImputation.filterBy(executionImputation, database),savables,deletables);
		result.setFromSavables(savables).setNumberOfDeletionFromCollection(deletables);		
		if(CollectionHelper.isNotEmpty(savables))
			__inject__(ScopeFunctionExecutionImputationBusiness.class).saveMany(savables);				
		if(CollectionHelper.isNotEmpty(deletables))
			__inject__(ScopeFunctionExecutionImputationBusiness.class).deleteMany(deletables);		
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
}