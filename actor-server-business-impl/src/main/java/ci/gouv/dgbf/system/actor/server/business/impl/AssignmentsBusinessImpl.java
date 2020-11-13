package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.enterprise.context.ApplicationScoped;
import javax.transaction.Transactional;

import org.cyk.utility.__kernel__.business.EntityCreator;
import org.cyk.utility.__kernel__.business.EntityUpdater;
import org.cyk.utility.__kernel__.business.TransactionResult;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.log.LogHelper;
import org.cyk.utility.__kernel__.number.NumberHelper;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.filter.Filter;
import org.cyk.utility.__kernel__.throwable.ThrowableHelper;
import org.cyk.utility.__kernel__.time.TimeHelper;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;
import org.cyk.utility.server.business.BusinessEntity;

import ci.gouv.dgbf.system.actor.server.business.api.AssignmentsBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.AssignmentsPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.AssignmentsQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ExecutionImputationQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

@ApplicationScoped
public class AssignmentsBusinessImpl extends AbstractBusinessEntityImpl<Assignments, AssignmentsPersistence> implements AssignmentsBusiness,Serializable {
	private static final long serialVersionUID = 1L;

	public static Integer INITIALIZE_EXECUTION_IMPUTATIONS_READ_BATCH_SIZE = 3000;
	public static Integer READ_BATCH_SIZE = 3000;
	
	//public static Integer INITIALIZE_EXECUTION_IMPUTATIONS_PROCESS_BATCH_SIZE = 25;
	
	/**
	 * L'initialisation consiste à récupérer les imputations (en préparation pour l'exécution) afin de les ajouter à la liste des affectations.
	 * NB : Aucune ligne n'est supprimées suite à l'exécution de cette fonction
	 */
	@Override
	public TransactionResult initialize() {
		TransactionResult transactionResult = new TransactionResult().setName("Initialisation").setTupleName("Affectation");
		//1 - get execution imputation identifiers not yet in assignments
		Long numberOfExecutionImputations = ExecutionImputationQuerier.getInstance().countIdentifiersNotInAssignments();
		if(NumberHelper.isLessThanOrEqualZero(numberOfExecutionImputations))
			return null;
		//2 - get scopes functions to assign
		Long numberOfScopeFunctions = ScopeFunctionQuerier.getInstance().count();
		LogHelper.logInfo(String.format("%s poste(s)",numberOfScopeFunctions), getClass());
		if(NumberHelper.isEqualToZero(numberOfScopeFunctions))
			return null;
		LogHelper.logInfo(String.format("Chargement de %s poste(s) en mémoire...",numberOfScopeFunctions), getClass());
		Collection<ScopeFunction> scopeFunctions = (List<ScopeFunction>) ScopeFunctionQuerier.getInstance().readAllWithReferencesOnly(new QueryExecutorArguments());
		LogHelper.logInfo(String.format("%s poste(s) chargé(s)",scopeFunctions.size()), getClass());		
		
		LogHelper.logInfo(String.format("Read batch size = %s",INITIALIZE_EXECUTION_IMPUTATIONS_READ_BATCH_SIZE), getClass());
		do {
			numberOfExecutionImputations = ExecutionImputationQuerier.getInstance().countIdentifiersNotInAssignments();
			LogHelper.logInfo(String.format("%s imputation(s) à initialiser", numberOfExecutionImputations), getClass());
			if(NumberHelper.isLessThanOrEqualZero(numberOfExecutionImputations))
				break;
			initialize(scopeFunctions,transactionResult);
		}while(true);
		
		transactionResult.log(getClass());
		return transactionResult;
	}

	private void initialize(Collection<ScopeFunction> scopeFunctions,TransactionResult transactionResult) {
		Long t = System.currentTimeMillis();
		Collection<ExecutionImputation> executionImputations = ExecutionImputationQuerier.getInstance().readNotInAssignmentsForInitialization(
				new QueryExecutorArguments().setNumberOfTuples(INITIALIZE_EXECUTION_IMPUTATIONS_READ_BATCH_SIZE));
		LogHelper.logInfo(String.format("\t%s imputation(s) chargée(s) en %s",CollectionHelper.getSize(executionImputations),TimeHelper.formatDuration(System.currentTimeMillis() - t)), getClass());
		if(CollectionHelper.isEmpty(executionImputations))
			return;
		t = System.currentTimeMillis();
		Collection<Object> collection = new ArrayList<>();	
		executionImputations.forEach(executionImputation -> {
			Assignments assignments = new Assignments().setExecutionImputation(executionImputation);
			assignments.setCreditManagerHolder(findScopeFunction(executionImputation.getAdministrativeUnitCode(), Function.CODE_CREDIT_MANAGER_HOLDER, scopeFunctions));
			assignments.setCreditManagerAssistant(findScopeFunction(executionImputation.getAdministrativeUnitCode(), Function.CODE_CREDIT_MANAGER_ASSISTANT, scopeFunctions));
			assignments.setAuthorizingOfficerHolder(findScopeFunction(executionImputation.getBudgetSpecializationUnitCode(), Function.CODE_AUTHORIZING_OFFICER_HOLDER, scopeFunctions));
			assignments.setAuthorizingOfficerAssistant(findScopeFunction(executionImputation.getBudgetSpecializationUnitCode(), Function.CODE_AUTHORIZING_OFFICER_ASSISTANT, scopeFunctions));
			assignments.setFinancialControllerHolder(findScopeFunction(executionImputation.getSectionCode(), Function.CODE_FINANCIAL_CONTROLLER_HOLDER, scopeFunctions));
			assignments.setFinancialControllerAssistant(findScopeFunction(executionImputation.getSectionCode(), Function.CODE_FINANCIAL_CONTROLLER_ASSISTANT, scopeFunctions));
			assignments.setAccountingHolder(findScopeFunction(executionImputation.getSectionCode(), Function.CODE_ACCOUNTING_HOLDER, scopeFunctions));
			assignments.setAccountingAssistant(findScopeFunction(executionImputation.getSectionCode(), Function.CODE_ACCOUNTING_ASSISTANT, scopeFunctions));
			collection.add(assignments);
		});
		LogHelper.logInfo(String.format("\tPostes attribués en %s",TimeHelper.formatDuration(System.currentTimeMillis() - t)), getClass());
		t = System.currentTimeMillis();
		EntityCreator.getInstance().createMany(collection);
		transactionResult.setNumberOfCreation(NumberHelper.add(transactionResult.getNumberOfCreation(),collection.size()).longValue());
		LogHelper.logInfo(String.format("\tCréation en %s",TimeHelper.formatDuration(System.currentTimeMillis() - t)), getClass());
	}
	
	private static ScopeFunction findScopeFunction(String scopeCode,String functionCode,Collection<ScopeFunction> scopeFunctions) {
		for(ScopeFunction scopeFunction : scopeFunctions)
			if(scopeFunction.getScopeAsString().equals(scopeCode) && scopeFunction.getFunctionAsString().equals(functionCode))
				return scopeFunction;
		return null;
	}
	
	/**
	 * Enregistre les modifications.
	 */
	@Transactional
	@Override
	public TransactionResult saveScopeFunctions(Collection<Assignments> collection) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("Assignments collection", collection);
		TransactionResult transactionResult = new TransactionResult().setName("Enregistrement").setTupleName("Affectation");
		EntityUpdater.getInstance().updateMany(CollectionHelper.cast(Object.class, collection));
		transactionResult.setNumberOfUpdateFromSavables(collection);
		transactionResult.log(getClass());
		return transactionResult;
	}

	/**
	 * La liste ,éligible sur la base du filtre, est modifiée avec les valeurs du modèle.
	 * NB : Si une valeur est non nulle alors elle sera écrasée si cela à été explicitement spécifié.
	 */
	@Override
	public TransactionResult applyModel(Assignments model, Filter filter, Collection<String> overridablesFieldsNames) {
		ThrowableHelper.throwIllegalArgumentExceptionIfNull("model", model);
		ThrowableHelper.throwIllegalArgumentExceptionIfNull("filter", filter);
		TransactionResult transactionResult = new TransactionResult().setName("Application de modèle").setTupleName("Affectation");
		LogHelper.logInfo(String.format("Options d'écrasement : %s", overridablesFieldsNames), getClass());
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments();
		queryExecutorArguments.setFilter(filter);
		queryExecutorArguments.setQueryFromIdentifier(AssignmentsQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER);		
		LogHelper.logInfo(String.format("Compte des affectations à traiter en cours..."), getClass());
		Long t = System.currentTimeMillis();
		Long numberOfExecutionImputations = AssignmentsQuerier.getInstance().countWhereFilter(queryExecutorArguments);
		LogHelper.logInfo(String.format("%s affectations à traiter compté en %s", numberOfExecutionImputations,TimeHelper.formatDuration(System.currentTimeMillis() - t)), getClass());
		if(NumberHelper.isLessThanOrEqualZero(numberOfExecutionImputations))
			return null;
		Integer numberOfBatches = (int) (numberOfExecutionImputations / READ_BATCH_SIZE) + (numberOfExecutionImputations % READ_BATCH_SIZE == 0 ? 0 : 1);
		LogHelper.logInfo(String.format("taille du lot est de %s. %s lot(s) à traiter",READ_BATCH_SIZE,numberOfBatches), getClass());
		queryExecutorArguments.setQueryFromIdentifier(AssignmentsQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER).setNumberOfTuples(READ_BATCH_SIZE);
		for(Integer index = 0; index < numberOfBatches; index = index + 1) {
			applyModel(model, overridablesFieldsNames, queryExecutorArguments.setFirstTupleIndex(index * READ_BATCH_SIZE),transactionResult);
			//TransactionResult r = deriveScopeFunctionsFromModel(executionImputationModel, queryExecutorArguments, batchSize, index*batchSize
			//		,DERIVE_SCOPE_FUNCTIONS_FROM_MODEL_EXECUTION_IMPUTATIONS_PROCESS_BATCH_SIZE);			
		}
		transactionResult.log(getClass());
		return transactionResult;
	}
	
	private void applyModel(Assignments model, Collection<String> overridablesFieldsNames,QueryExecutorArguments queryExecutorArguments,TransactionResult transactionResult) {
		Long t = System.currentTimeMillis();
		Collection<Assignments> collection = AssignmentsQuerier.getInstance().readWhereFilterForApplyModel(queryExecutorArguments);	
		LogHelper.logInfo(String.format("\tChargement de %s assignation(s) à partir l'index %s en %s",CollectionHelper.getSize(collection)
				,queryExecutorArguments.getFirstTupleIndex(),TimeHelper.formatDuration(System.currentTimeMillis() - t)), getClass());
		if(CollectionHelper.isEmpty(collection))
			return;
		collection.forEach(index -> {
			if(index.getCreditManagerHolder() == null || CollectionHelper.contains(overridablesFieldsNames, Assignments.FIELD_CREDIT_MANAGER_HOLDER))
				index.setCreditManagerHolder(model.getCreditManagerHolder());
			if(index.getCreditManagerAssistant() == null || CollectionHelper.contains(overridablesFieldsNames, Assignments.FIELD_CREDIT_MANAGER_ASSISTANT))
				index.setCreditManagerAssistant(model.getCreditManagerAssistant());
			
			if(index.getAuthorizingOfficerHolder() == null || CollectionHelper.contains(overridablesFieldsNames, Assignments.FIELD_AUTHORIZING_OFFICER_HOLDER))
				index.setAuthorizingOfficerHolder(model.getAuthorizingOfficerHolder());
			if(index.getAuthorizingOfficerAssistant() == null || CollectionHelper.contains(overridablesFieldsNames, Assignments.FIELD_AUTHORIZING_OFFICER_ASSISTANT))
				index.setAuthorizingOfficerAssistant(model.getAuthorizingOfficerAssistant());
			
			if(index.getFinancialControllerHolder() == null || CollectionHelper.contains(overridablesFieldsNames, Assignments.FIELD_FINANCIAL_CONTROLLER_HOLDER))
				index.setFinancialControllerHolder(model.getFinancialControllerHolder());
			if(index.getFinancialControllerAssistant() == null || CollectionHelper.contains(overridablesFieldsNames, Assignments.FIELD_FINANCIAL_CONTROLLER_ASSISTANT))
				index.setFinancialControllerAssistant(model.getFinancialControllerAssistant());
			
			if(index.getAccountingHolder() == null || CollectionHelper.contains(overridablesFieldsNames, Assignments.FIELD_ACCOUNTING_HOLDER))
				index.setAccountingHolder(model.getAccountingHolder());
			if(index.getAccountingAssistant() == null || CollectionHelper.contains(overridablesFieldsNames, Assignments.FIELD_ACCOUNTING_ASSISTANT))
				index.setAccountingAssistant(model.getAccountingAssistant());
		});
		t = System.currentTimeMillis();
		EntityUpdater.getInstance().updateMany(CollectionHelper.cast(Object.class, collection));
		LogHelper.logInfo(String.format("\tEnregistrement en %s",TimeHelper.formatDuration(System.currentTimeMillis() - t)), getClass());
		transactionResult.setNumberOfUpdate(NumberHelper.add(transactionResult.getNumberOfUpdate(),collection.size()).longValue());
	}

	@Override @Transactional
	public BusinessEntity<Assignments> deleteAll() {
		QueryExecutor.getInstance().executeUpdateOrDelete(new QueryExecutorArguments().setQuery(new Query().setValue("DELETE FROM Assignments")));
		return this;
	}
}