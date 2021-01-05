package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

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
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.throwable.ThrowableHelper;
import org.cyk.utility.__kernel__.time.TimeHelper;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;
import org.cyk.utility.server.business.BusinessEntity;

import ci.gouv.dgbf.system.actor.server.business.api.AssignmentsBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.AssignmentsPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.AccountingServiceQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.AssignmentsQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.AuthorizingOfficerServiceQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ExecutionImputationQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.FinancialControllerServiceQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AccountingService;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AuthorizingOfficerService;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.FinancialControllerService;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

@ApplicationScoped
public class AssignmentsBusinessImpl extends AbstractBusinessEntityImpl<Assignments, AssignmentsPersistence> implements AssignmentsBusiness,Serializable {
	private static final long serialVersionUID = 1L;

	public static Integer INITIALIZE_EXECUTION_IMPUTATIONS_READ_BATCH_SIZE = 15000;
	public static Integer READ_BATCH_SIZE = 10000;
	
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
		Collection<ScopeFunction> scopeFunctions = ScopeFunctionQuerier.getInstance().readAllWithReferencesOnly(new QueryExecutorArguments());
		LogHelper.logInfo(String.format("%s poste(s) chargé(s)",scopeFunctions.size()), getClass());
		
		//3 - get authorizing officer services to assign
		Long numberOfAuthorizingOfficerServices = AuthorizingOfficerServiceQuerier.getInstance().countAll();
		LogHelper.logInfo(String.format("Chargement de %s service(s) d'ordonnateur(s) en mémoire...",numberOfAuthorizingOfficerServices), getClass());
		Collection<AuthorizingOfficerService> authorizingOfficerServices = AuthorizingOfficerServiceQuerier.getInstance().readAllForAssignmentsInitialization();
		LogHelper.logInfo(String.format("%s service(s) d'ordonnateur(s) chargé(s)",authorizingOfficerServices.size()), getClass());
		
		//4 - get financial controller services to assign
		Long numberOfFinancialControllerServices = FinancialControllerServiceQuerier.getInstance().countAll();
		LogHelper.logInfo(String.format("Chargement de %s service(s) de controleur(s) financier(s) en mémoire...",numberOfFinancialControllerServices), getClass());
		Collection<FinancialControllerService> financialControllerServices = FinancialControllerServiceQuerier.getInstance().readAllForAssignmentsInitialization();
		LogHelper.logInfo(String.format("%s service(s) de controleur(s) financier(s) chargé(s)",financialControllerServices.size()), getClass());
		
		//5 - get accounting services to assign
		Long numberOfAccountingServices = AccountingServiceQuerier.getInstance().countAll();
		LogHelper.logInfo(String.format("Chargement de %s service(s) de comptable(s) en mémoire...",numberOfAccountingServices), getClass());
		Collection<AccountingService> accountingServices = AccountingServiceQuerier.getInstance().readAllForAssignmentsInitialization();
		LogHelper.logInfo(String.format("%s service(s) de comptable(s) chargé(s)",accountingServices.size()), getClass());
		
		LogHelper.logInfo(String.format("Read batch size = %s",INITIALIZE_EXECUTION_IMPUTATIONS_READ_BATCH_SIZE), getClass());
		do {
			numberOfExecutionImputations = ExecutionImputationQuerier.getInstance().countIdentifiersNotInAssignments();
			LogHelper.logInfo(String.format("%s imputation(s) à initialiser", numberOfExecutionImputations), getClass());
			if(NumberHelper.isLessThanOrEqualZero(numberOfExecutionImputations))
				break;
			initialize(scopeFunctions,authorizingOfficerServices,financialControllerServices,accountingServices,transactionResult);
			System.gc();
			//break;
		}while(true);
		scopeFunctions.clear();
		scopeFunctions = null;
		authorizingOfficerServices.clear();
		authorizingOfficerServices = null;
		financialControllerServices.clear();
		financialControllerServices = null;
		accountingServices.clear();
		accountingServices = null;
		
		transactionResult.log(getClass());
		return transactionResult;
	}

	private void initialize(Collection<ScopeFunction> scopeFunctions,Collection<AuthorizingOfficerService> authorizingOfficerServices
			,Collection<FinancialControllerService> financialControllerServices,Collection<AccountingService> accountingServices
			,TransactionResult transactionResult) {
		Long t = System.currentTimeMillis();
		Collection<ExecutionImputation> executionImputations = ExecutionImputationQuerier.getInstance().readNotInAssignmentsForInitialization(
				new QueryExecutorArguments().setNumberOfTuples(INITIALIZE_EXECUTION_IMPUTATIONS_READ_BATCH_SIZE));
		LogHelper.logInfo(String.format("\t%s imputation(s) chargée(s) en %s",CollectionHelper.getSize(executionImputations),TimeHelper.formatDuration(System.currentTimeMillis() - t)), getClass());
		if(CollectionHelper.isEmpty(executionImputations))
			return;
		t = System.currentTimeMillis();
		Collection<Assignments> collection = new ArrayList<>();
		for(ExecutionImputation executionImputation : executionImputations)
			collection.add(new Assignments().setExecutionImputation(executionImputation));
		executionImputations.clear();
		
		collection.parallelStream().forEach(assignments -> {
			setScopeFunctions(assignments, scopeFunctions,authorizingOfficerServices,financialControllerServices,accountingServices);		
		});		
		LogHelper.logInfo(String.format("\tPostes attribués en %s",TimeHelper.formatDuration(System.currentTimeMillis() - t)), getClass());
		t = System.currentTimeMillis();
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments();
		queryExecutorArguments.addObjects(CollectionHelper.cast(Object.class, collection));
		queryExecutorArguments.setIsEntityManagerFlushable(Boolean.TRUE).setIsEntityManagerClearable(Boolean.TRUE).setIsEntityManagerClosable(Boolean.TRUE);
		EntityCreator.getInstance().create(queryExecutorArguments); //USE NATIVE SQL
		
		/*
		IdentifiableSystem.setManyIfNull(collection);
		String sql = __inject__(NativeQueryStringBuilder.class).buildInsertMany(Assignments.class, CollectionHelper.cast(Assignments.class, collection));
		//System.out.println(sql);
		__inject__(NativeQueryStringExecutor.class).execute(new org.cyk.utility.__kernel__.persistence.query.NativeQueryStringExecutor.Arguments().addQueriesStrings(sql));
		*/
		transactionResult.setNumberOfCreation(NumberHelper.add(transactionResult.getNumberOfCreation(),collection.size()).longValue());
		collection.clear();
		LogHelper.logInfo(String.format("\tCréation en %s",TimeHelper.formatDuration(System.currentTimeMillis() - t)), getClass());
	}
	
	private static void setScopeFunctions(Assignments assignments,Collection<ScopeFunction> scopeFunctions
			,Collection<AuthorizingOfficerService> authorizingOfficerServices,Collection<FinancialControllerService> financialControllerServices
			,Collection<AccountingService> accountingServices) {
		String managerCode = assignments.getExecutionImputation().getManagerCode();
		if(StringHelper.isBlank(managerCode))
			managerCode = assignments.getExecutionImputation().getAdministrativeUnitCode();
		assignments.setCreditManagerHolder(findCreditManagerHolderScopeFunction(managerCode, scopeFunctions));
		assignments.setCreditManagerAssistant(findAssistantScopeFunction(assignments.getCreditManagerHolder(),Function.CODE_CREDIT_MANAGER_ASSISTANT, scopeFunctions));
		
		assignments.setAuthorizingOfficerHolder(findAuthorizingOfficerServiceHolderScopeFunction(managerCode
				,assignments.getExecutionImputation().getManagerLocalityCode()
				,assignments.getExecutionImputation().getBudgetSpecializationUnitCode()
				,authorizingOfficerServices, scopeFunctions));
		assignments.setAuthorizingOfficerAssistant(findAssistantScopeFunction(assignments.getAuthorizingOfficerHolder(),Function.CODE_AUTHORIZING_OFFICER_ASSISTANT, scopeFunctions));
		
		assignments.setFinancialControllerHolder(findFinancialControllerServiceHolderScopeFunction(managerCode
				,assignments.getExecutionImputation().getSectionCode()
				,assignments.getExecutionImputation().getManagerLocalityCode(), financialControllerServices, scopeFunctions));
		assignments.setFinancialControllerAssistant(findAssistantScopeFunction(assignments.getFinancialControllerHolder(), Function.CODE_FINANCIAL_CONTROLLER_ASSISTANT, scopeFunctions));
		
		assignments.setAccountingHolder(findAccountingServiceHolderScopeFunction(managerCode
				,assignments.getExecutionImputation().getSectionCode()
				,assignments.getExecutionImputation().getManagerLocalityCode(), accountingServices, scopeFunctions));
		assignments.setAccountingAssistant(findAssistantScopeFunction(assignments.getFinancialControllerHolder(), Function.CODE_ACCOUNTING_ASSISTANT, scopeFunctions));
	}
	
	private static ScopeFunction findCreditManagerHolderScopeFunction(String managerCode,Collection<ScopeFunction> scopeFunctions) {
		for(ScopeFunction scopeFunction : scopeFunctions) {
			if(scopeFunction.getScopeCode().equals(managerCode) && scopeFunction.getFunctionAsString().equals(Function.CODE_CREDIT_MANAGER_HOLDER))
				return scopeFunction;
		}
		return null;
	}
	
	private static ScopeFunction findAssistantScopeFunction(ScopeFunction holder,String assistantFunctionCode,Collection<ScopeFunction> scopeFunctions) {
		if(holder == null)
			return null;
		for(ScopeFunction scopeFunction : scopeFunctions) {
			if(scopeFunction.getFunctionAsString().equals(assistantFunctionCode) && scopeFunction.getScopeCode().startsWith(holder.getCode()))
				return scopeFunction;
		}
		return null;
	}
	
	private static ScopeFunction findAuthorizingOfficerServiceHolderScopeFunction(String managerCode,String localityCode,String budgetSpecializationUnitCode
			,Collection<AuthorizingOfficerService> authorizingOfficerServices,Collection<ScopeFunction> scopeFunctions) {
		if(StringHelper.isBlank(managerCode))
			return null;
		if(managerCode.startsWith("1")) {
			//Find Délégué
			for(AuthorizingOfficerService authorizingOfficerService : authorizingOfficerServices) {
				if(authorizingOfficerService.getBudgetSpecializationUnitCode().equals(budgetSpecializationUnitCode) 
						&& StringHelper.isBlank(authorizingOfficerService.getLocalityCode())) {
					for(ScopeFunction scopeFunction : scopeFunctions)
						if(scopeFunction.getScopeIdentifier().equals(authorizingOfficerService.getIdentifier()))
							return scopeFunction;
					break;
				}
			}
		}else {
			//Find Secondaire
			for(AuthorizingOfficerService authorizingOfficerService : authorizingOfficerServices) {
				if(authorizingOfficerService.getBudgetSpecializationUnitCode().equals(budgetSpecializationUnitCode) 
						&& StringHelper.isNotBlank(localityCode) && localityCode.equals(authorizingOfficerService.getLocalityCode())) {
					for(ScopeFunction scopeFunction : scopeFunctions)
						if(scopeFunction.getScopeIdentifier().equals(authorizingOfficerService.getIdentifier()))
							return scopeFunction;
					break;
				}
			}
		}
		/*
		for(AuthorizingOfficerService authorizingOfficerService : authorizingOfficerServices) {		
			if(Locality.CODE_SOUS_PREFECTURE_BINGERVILLE.equals(localityCode)) {
				if(authorizingOfficerService.getBudgetSpecializationUnitCode().equals(budgetSpecializationUnitCode) 
						&& StringHelper.isBlank(authorizingOfficerService.getLocalityCode())) {
					for(ScopeFunction scopeFunction : scopeFunctions)
						if(scopeFunction.getScopeIdentifier().equals(authorizingOfficerService.getIdentifier()))
							return scopeFunction;
					break;
				}
			}else {
				if(authorizingOfficerService.getBudgetSpecializationUnitCode().equals(budgetSpecializationUnitCode) 
						&& StringHelper.isNotBlank(localityCode) && localityCode.equals(authorizingOfficerService.getLocalityCode())) {
					for(ScopeFunction scopeFunction : scopeFunctions)
						if(scopeFunction.getScopeIdentifier().equals(authorizingOfficerService.getIdentifier()))
							return scopeFunction;
					break;
				}
			}			
		}
		*/
		return null;
	}
	
	private static ScopeFunction findFinancialControllerServiceHolderScopeFunction(String managerCode,String sectionCode,String localityCode
			,Collection<FinancialControllerService> financialControllerServices,Collection<ScopeFunction> scopeFunctions) {
		if(sectionCode.startsWith("1")) {
			//Find institution's financial controller service
			for(FinancialControllerService financialControllerService : financialControllerServices) {
				if(financialControllerService.getCode().equals(FinancialControllerService.CODE_INSTITUTIONS)) {
					for(ScopeFunction scopeFunction : scopeFunctions)
						if(scopeFunction.getScopeIdentifier().equals(financialControllerService.getIdentifier()))
							return scopeFunction;
					break;
				}
			}
		}else {
			if(managerCode.startsWith("1")) {
				//Find section's financial controller service
				for(FinancialControllerService financialControllerService : financialControllerServices) {
					if(StringHelper.isNotBlank(sectionCode) && sectionCode.equals(financialControllerService.getSectionCode())) {
						for(ScopeFunction scopeFunction : scopeFunctions)
							if(scopeFunction.getScopeIdentifier().equals(financialControllerService.getIdentifier()))
								return scopeFunction;
						break;
					}
				}
			}else {
				//Find locality's financial controller service
				for(FinancialControllerService financialControllerService : financialControllerServices) {
					if(StringHelper.isNotBlank(localityCode) && localityCode.equals(financialControllerService.getLocalityCode())) {
						for(ScopeFunction scopeFunction : scopeFunctions)
							if(scopeFunction.getScopeIdentifier().equals(financialControllerService.getIdentifier()))
								return scopeFunction;
						break;
					}	
				}
			}
		}
		
		/*
		for(FinancialControllerService financialControllerService : financialControllerServices) {
			if(Locality.CODE_SOUS_PREFECTURE_BINGERVILLE.equals(localityCode)) {
				
			}else {
				if(StringHelper.isNotBlank(localityCode) && localityCode.equals(financialControllerService.getLocalityCode())) {
					for(ScopeFunction scopeFunction : scopeFunctions)
						if(scopeFunction.getScopeIdentifier().equals(financialControllerService.getIdentifier()))
							return scopeFunction;
					break;
				}
			}			
		}
		*/
		return null;
	}
	
	private static ScopeFunction findAccountingServiceHolderScopeFunction(String managerCode,String sectionCode,String localityCode
			,Collection<AccountingService> accountingServices,Collection<ScopeFunction> scopeFunctions) {
		
		if(managerCode.startsWith("1")) {
			
		}else {
			//Find locality's accounting service
			for(AccountingService accountingService : accountingServices) {
				if(StringHelper.isNotBlank(localityCode) && localityCode.equals(accountingService.getLocalityCode())) {
					for(ScopeFunction scopeFunction : scopeFunctions)
						if(scopeFunction.getScopeIdentifier().equals(accountingService.getIdentifier()))
							return scopeFunction;
					break;
				}
			}
		}
		
		/*
		for(AccountingService accountingService : accountingServices) {
			if(Locality.CODE_SOUS_PREFECTURE_BINGERVILLE.equals(localityCode)) {
				
			}else {
				if(StringHelper.isNotBlank(localityCode) && localityCode.equals(accountingService.getLocalityCode())) {
					for(ScopeFunction scopeFunction : scopeFunctions)
						if(scopeFunction.getScopeIdentifier().equals(accountingService.getIdentifier()))
							return scopeFunction;
					break;
				}
			}			
		}
		*/
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
		collection.parallelStream().forEach(index -> {
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
		
		QueryExecutorArguments updaterQueryExecutorArguments = new QueryExecutorArguments();
		updaterQueryExecutorArguments.addObjects(CollectionHelper.cast(Object.class, collection));
		updaterQueryExecutorArguments.setIsEntityManagerFlushable(Boolean.TRUE).setIsEntityManagerClearable(Boolean.TRUE).setIsEntityManagerClosable(Boolean.TRUE);
		EntityUpdater.getInstance().update(updaterQueryExecutorArguments);
		LogHelper.logInfo(String.format("\tEnregistrement en %s",TimeHelper.formatDuration(System.currentTimeMillis() - t)), getClass());
		transactionResult.setNumberOfUpdate(NumberHelper.add(transactionResult.getNumberOfUpdate(),collection.size()).longValue());
		collection.clear();
		collection = null;
	}

	@Override @Transactional
	public BusinessEntity<Assignments> deleteAll() {
		QueryExecutor.getInstance().executeUpdateOrDelete(new QueryExecutorArguments().setQuery(new Query().setValue("DELETE FROM Assignments")));
		return this;
	}
}