package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import javax.enterprise.context.ApplicationScoped;
import javax.persistence.EntityManager;
import javax.transaction.Transactional;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.log.LogHelper;
import org.cyk.utility.__kernel__.number.NumberHelper;
import org.cyk.utility.__kernel__.object.__static__.persistence.EntityLifeCycleListener;
import org.cyk.utility.__kernel__.object.marker.IdentifiableSystem;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.throwable.ThrowableHelper;
import org.cyk.utility.__kernel__.time.TimeHelper;
import org.cyk.utility.__kernel__.value.ValueHelper;
import org.cyk.utility.business.TransactionResult;
import org.cyk.utility.business.server.EntityCreator;
import org.cyk.utility.business.server.EntityUpdater;
import org.cyk.utility.persistence.query.EntityFinder;
import org.cyk.utility.persistence.query.Filter;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutor;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.server.query.ReaderByCollection;
import org.cyk.utility.persistence.server.query.executor.field.CodeExecutor;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;
import org.cyk.utility.server.business.BusinessEntity;

import ci.gouv.dgbf.system.actor.server.business.api.AssignmentsBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.AssignmentsPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.ScopeFunctionPersistence;
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
	public static Integer DERIVE_VALUES_READ_BATCH_SIZE = 15000;
	public static Integer READ_BATCH_SIZE = 10000;
	public static Boolean EXPORT = Boolean.TRUE;
	
	//public static Integer INITIALIZE_EXECUTION_IMPUTATIONS_PROCESS_BATCH_SIZE = 25;
	
	/**
	 * L'initialisation consiste à récupérer les imputations (en préparation pour l'exécution) afin de les ajouter à la liste des affectations.
	 * NB : Aucune ligne n'est supprimées suite à l'exécution de cette fonction
	 */
	@Override
	public TransactionResult initialize(String actorCode) {
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
			initialize(Boolean.TRUE,Boolean.TRUE,Boolean.TRUE,actorCode,scopeFunctions,authorizingOfficerServices,financialControllerServices,accountingServices,transactionResult);
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

	@Override
	@Transactional
	public TransactionResult deriveValues(Collection<Assignments> collection,Boolean holdersSettable,Boolean assistantsSettable, Boolean overridable,String actorCode) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("affectations", collection);
		TransactionResult transactionResult = new TransactionResult().setName("Dérivation des postes").setTupleName("Affectations");
		
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
		
		deriveValues(collection,holdersSettable,assistantsSettable,overridable,actorCode,"dérivation", scopeFunctions, authorizingOfficerServices, financialControllerServices, accountingServices);
		__persistence__.updateMany(collection);
		transactionResult.setNumberOfUpdateFromSavables(collection);
		transactionResult.log(getClass());
		return transactionResult;
	}
	
	@Override
	public TransactionResult deriveAllValues(Boolean holdersSettable,Boolean assistantsSettable,Boolean overridable,String actorCode) {
		TransactionResult transactionResult = new TransactionResult().setName("Dérivation des postes").setTupleName("Affectations");
		LogHelper.logInfo(String.format("Dériver les valeurs. titulaire=%s , assistant=%s , écraser=%s",holdersSettable,assistantsSettable,overridable), getClass());
		
		LogHelper.logInfo(String.format("Compte des affectations à traiter en cours..."), getClass());
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments();
		queryExecutorArguments.setQueryFromIdentifier(AssignmentsQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER);
		Long t = System.currentTimeMillis();
		Long numberOfAssignments = AssignmentsQuerier.getInstance().countWhereFilter(null);
		LogHelper.logInfo(String.format("%s affectations à traiter compté en %s", numberOfAssignments,TimeHelper.formatDuration(System.currentTimeMillis() - t)), getClass());
		if(NumberHelper.isLessThanOrEqualZero(numberOfAssignments))
			return null;
		Integer numberOfBatches = (int) (numberOfAssignments / DERIVE_VALUES_READ_BATCH_SIZE) + (numberOfAssignments % DERIVE_VALUES_READ_BATCH_SIZE == 0 ? 0 : 1);
		LogHelper.logInfo(String.format("taille du lot est de %s. %s lot(s) à traiter",DERIVE_VALUES_READ_BATCH_SIZE,numberOfBatches), getClass());
		queryExecutorArguments.setQueryFromIdentifier(AssignmentsQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER).setNumberOfTuples(DERIVE_VALUES_READ_BATCH_SIZE);
		
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
		
		LogHelper.logInfo(String.format("Read batch size = %s",DERIVE_VALUES_READ_BATCH_SIZE), getClass());
		
		for(Integer index = 0; index < numberOfBatches; index = index + 1) {
			deriveAllValues(holdersSettable,assistantsSettable,overridable,actorCode,scopeFunctions,authorizingOfficerServices,financialControllerServices,accountingServices
					, queryExecutorArguments.setFirstTupleIndex(index * DERIVE_VALUES_READ_BATCH_SIZE),transactionResult);
		}		
		transactionResult.log(getClass());
		return transactionResult;
	}
	
	private void deriveAllValues(Boolean holdersSettable,Boolean assistantsSettable,Boolean overridable,String actorCode,Collection<ScopeFunction> scopeFunctions
			,Collection<AuthorizingOfficerService> authorizingOfficerServices,Collection<FinancialControllerService> financialControllerServices
			,Collection<AccountingService> accountingServices,QueryExecutorArguments queryExecutorArguments,TransactionResult transactionResult) {
		Long t = System.currentTimeMillis();
		Collection<Assignments> collection = AssignmentsQuerier.getInstance().readWhereFilterForApplyModel(queryExecutorArguments);	
		LogHelper.logInfo(String.format("\tChargement de %s affectation(s) à partir l'index %s en %s",CollectionHelper.getSize(collection)
				,queryExecutorArguments.getFirstTupleIndex(),TimeHelper.formatDuration(System.currentTimeMillis() - t)), getClass());
		if(CollectionHelper.isEmpty(collection))
			return;
		deriveValues(collection,holdersSettable,assistantsSettable,overridable,actorCode,"dérivation", scopeFunctions, authorizingOfficerServices, financialControllerServices, accountingServices);
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
	
	private void deriveValues(Collection<Assignments> collection,Boolean holdersSettable,Boolean assistantsSettable,Boolean overridable,String actorCode,String functionality,Collection<ScopeFunction> scopeFunctions
			,Collection<AuthorizingOfficerService> authorizingOfficerServices,Collection<FinancialControllerService> financialControllerServices
			,Collection<AccountingService> accountingServices) {
		if(CollectionHelper.isEmpty(collection))
			return;
		Long t = System.currentTimeMillis();
		LogHelper.logInfo(String.format("\tDerivation des valeurs de %s ligne(s) d'affectation(s)",collection.size()), getClass());
		collection.parallelStream().forEach(assignments -> {
			assignments.set__auditWho__(actorCode);
			assignments.set__auditFunctionality__(functionality);
			setScopeFunctions(assignments,holdersSettable,assistantsSettable,overridable, scopeFunctions,authorizingOfficerServices,financialControllerServices,accountingServices);		
		});
		LogHelper.logInfo(String.format("\t%s ligne(s) d'affectation(s) dérivée(s) en %s",collection.size(),TimeHelper.formatDuration(System.currentTimeMillis() - t)), getClass());
	}
	
	private void initialize(Boolean holdersSettable,Boolean assistantsSettable,Boolean overridable,String actorCode,Collection<ScopeFunction> scopeFunctions,Collection<AuthorizingOfficerService> authorizingOfficerServices
			,Collection<FinancialControllerService> financialControllerServices,Collection<AccountingService> accountingServices
			,TransactionResult transactionResult) {
		Long t = System.currentTimeMillis();
		Collection<ExecutionImputation> executionImputations = ExecutionImputationQuerier.getInstance().readNotInAssignmentsForInitialization(
				new QueryExecutorArguments().setNumberOfTuples(INITIALIZE_EXECUTION_IMPUTATIONS_READ_BATCH_SIZE));
		LogHelper.logInfo(String.format("\t%s imputation(s) chargée(s) en %s",CollectionHelper.getSize(executionImputations),TimeHelper.formatDuration(System.currentTimeMillis() - t)), getClass());
		if(CollectionHelper.isEmpty(executionImputations))
			return;
		LogHelper.logInfo(String.format("\tInstantiation des ligne(s) d'affectation(s)"), getClass());
		t = System.currentTimeMillis();		
		Collection<Assignments> collection = new ArrayList<>();
		for(ExecutionImputation executionImputation : executionImputations) {
			Assignments assignments = new Assignments().setIdentifier(executionImputation.getReferencedIdentifier()).setExecutionImputation(executionImputation);
			collection.add(assignments);
		}
		LogHelper.logInfo(String.format("\tLigne(s) instantiée(s) en %s",TimeHelper.formatDuration(System.currentTimeMillis() - t)), getClass());
		executionImputations.clear();
		
		deriveValues(collection,holdersSettable,assistantsSettable,overridable,actorCode,"initialisation", scopeFunctions, authorizingOfficerServices, financialControllerServices, accountingServices);
		
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments();
		queryExecutorArguments.addObjects(CollectionHelper.cast(Object.class, collection));
		queryExecutorArguments.setIsEntityManagerFlushable(Boolean.TRUE).setIsEntityManagerClearable(Boolean.TRUE).setIsEntityManagerClosable(Boolean.TRUE);
		LogHelper.logInfo(String.format("\tCréation de %s ligne(s) d'affectation(s)",collection.size()), getClass());
		t = System.currentTimeMillis();
		EntityCreator.getInstance().create(queryExecutorArguments);
		LogHelper.logInfo(String.format("\t%s ligne(s) d'affectation(s) créée(s) en %s",collection.size(),TimeHelper.formatDuration(System.currentTimeMillis() - t)), getClass());
		transactionResult.setNumberOfCreation(NumberHelper.add(transactionResult.getNumberOfCreation(),collection.size()).longValue());
		collection.clear();
		
	}
	
	private static void setScopeFunctions(Assignments assignments,Boolean holdersSettable,Boolean assistantsSettable,Boolean overridable,Collection<ScopeFunction> scopeFunctions
			,Collection<AuthorizingOfficerService> authorizingOfficerServices,Collection<FinancialControllerService> financialControllerServices
			,Collection<AccountingService> accountingServices) {
		String managerCode = assignments.getExecutionImputation().getManagerCode();
		if(StringHelper.isBlank(managerCode))
			managerCode = assignments.getExecutionImputation().getAdministrativeUnitCode();
		if(StringHelper.isBlank(managerCode)) {
			LogHelper.logWarning(String.format("Aucun gestionnaire trouvé sur la ligne %s",assignments.getExecutionImputation().getReferencedIdentifier()), AssignmentsBusinessImpl.class);
			return;
		}
		if(Boolean.TRUE.equals(holdersSettable)) {
			if(assignments.getCreditManagerHolder() == null || Boolean.TRUE.equals(overridable))
				assignments.setCreditManagerHolder(findCreditManagerHolderScopeFunction(managerCode, scopeFunctions));
			if(assignments.getAuthorizingOfficerHolder() == null || Boolean.TRUE.equals(overridable))
				assignments.setAuthorizingOfficerHolder(findAuthorizingOfficerServiceHolderScopeFunction(managerCode
					,assignments.getExecutionImputation().getManagerLocalityCode()
					,assignments.getExecutionImputation().getBudgetSpecializationUnitCode()
					,authorizingOfficerServices, scopeFunctions));		
			if(assignments.getFinancialControllerHolder() == null || Boolean.TRUE.equals(overridable))
				assignments.setFinancialControllerHolder(findFinancialControllerServiceHolderScopeFunction(managerCode
					,assignments.getExecutionImputation().getSectionCode()
					,assignments.getExecutionImputation().getManagerLocalityCode(), financialControllerServices, scopeFunctions));
			if(assignments.getAccountingHolder() == null || Boolean.TRUE.equals(overridable))
				assignments.setAccountingHolder(findAccountingServiceHolderScopeFunction(managerCode
					,assignments.getExecutionImputation().getSectionCode()
					,assignments.getExecutionImputation().getManagerLocalityCode(), accountingServices, scopeFunctions));
		}
		if(Boolean.TRUE.equals(assistantsSettable)) {
			if(assignments.getCreditManagerAssistant() == null || Boolean.TRUE.equals(overridable))
				assignments.setCreditManagerAssistant(findAssistantScopeFunction(assignments.getCreditManagerHolder(),Function.CODE_CREDIT_MANAGER_ASSISTANT, scopeFunctions));
			if(assignments.getAuthorizingOfficerAssistant() == null || Boolean.TRUE.equals(overridable))
				assignments.setAuthorizingOfficerAssistant(findAssistantScopeFunction(assignments.getAuthorizingOfficerHolder(),Function.CODE_AUTHORIZING_OFFICER_ASSISTANT, scopeFunctions));
			if(assignments.getFinancialControllerAssistant() == null || Boolean.TRUE.equals(overridable))
				assignments.setFinancialControllerAssistant(findAssistantScopeFunction(assignments.getFinancialControllerHolder(), Function.CODE_FINANCIAL_CONTROLLER_ASSISTANT, scopeFunctions));
			if(assignments.getAccountingAssistant() == null || Boolean.TRUE.equals(overridable))
				assignments.setAccountingAssistant(findAssistantScopeFunction(assignments.getAccountingHolder(), Function.CODE_ACCOUNTING_ASSISTANT, scopeFunctions));
		}
	}
	
	private static ScopeFunction findCreditManagerHolderScopeFunction(String managerCode,Collection<ScopeFunction> scopeFunctions) {
		for(ScopeFunction scopeFunction : scopeFunctions) {
			if(scopeFunction.getScopeCode().equals(managerCode) && scopeFunction.getFunctionAsString().equals(Function.CODE_CREDIT_MANAGER_HOLDER))
				return scopeFunction;
		}
		return null;
	}
	
	private static String computeAssistantCodeFromHolderCode(String holderCode,Integer index) {
		return "A"+index+holderCode.substring(2)+"0";
	}
	
	private static ScopeFunction findAssistantScopeFunction(ScopeFunction holder,String assistantFunctionCode,Collection<ScopeFunction> scopeFunctions) {
		if(holder == null)
			return null;
		for(ScopeFunction scopeFunction : scopeFunctions) {
			if(scopeFunction.getFunctionAsString().equals(assistantFunctionCode) && scopeFunction.getCode().substring(2).equals(holder.getCode().substring(2)+"0")) {
				return scopeFunction;
			}
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
						if(scopeFunction.getFunctionAsString().equals(Function.CODE_AUTHORIZING_OFFICER_HOLDER) && scopeFunction.getScopeIdentifier().equals(authorizingOfficerService.getIdentifier()))
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
						if(scopeFunction.getFunctionAsString().equals(Function.CODE_AUTHORIZING_OFFICER_HOLDER) && scopeFunction.getScopeIdentifier().equals(authorizingOfficerService.getIdentifier()))
							return scopeFunction;
					break;
				}
			}
			//Secondaire Not Found -> Délégué
			for(AuthorizingOfficerService authorizingOfficerService : authorizingOfficerServices) {
				if(authorizingOfficerService.getBudgetSpecializationUnitCode().equals(budgetSpecializationUnitCode) 
						&& StringHelper.isBlank(authorizingOfficerService.getLocalityCode())) {
					for(ScopeFunction scopeFunction : scopeFunctions)
						if(scopeFunction.getFunctionAsString().equals(Function.CODE_AUTHORIZING_OFFICER_HOLDER) && scopeFunction.getScopeIdentifier().equals(authorizingOfficerService.getIdentifier()))
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
						if(scopeFunction.getFunctionAsString().equals(Function.CODE_FINANCIAL_CONTROLLER_HOLDER) && scopeFunction.getScopeIdentifier().equals(financialControllerService.getIdentifier()))
							return scopeFunction;
					break;
				}
			}
		}else {
			if(managerCode.startsWith("1")) {
				//Find section's financial controller service
				for(FinancialControllerService financialControllerService : financialControllerServices) {
					if(StringHelper.isBlank(financialControllerService.getLocalityCode()) && StringHelper.isNotBlank(sectionCode) 
							&& sectionCode.equals(financialControllerService.getSectionCode()) ) {
						for(ScopeFunction scopeFunction : scopeFunctions)
							if(scopeFunction.getFunctionAsString().equals(Function.CODE_FINANCIAL_CONTROLLER_HOLDER) 
									&& scopeFunction.getScopeIdentifier().equals(financialControllerService.getIdentifier())							 ) {
								return scopeFunction;
							}
						break;
					}
				}
			}else {
				//Find locality's financial controller service
				for(FinancialControllerService financialControllerService : financialControllerServices) {
					if(StringHelper.isNotBlank(localityCode) && localityCode.equals(financialControllerService.getLocalityCode())) {
						for(ScopeFunction scopeFunction : scopeFunctions)
							if(scopeFunction.getFunctionAsString().equals(Function.CODE_FINANCIAL_CONTROLLER_HOLDER) && scopeFunction.getScopeIdentifier().equals(financialControllerService.getIdentifier()))
								return scopeFunction;
						break;
					}	
				}
				
				//Find section's financial controller service
				for(FinancialControllerService financialControllerService : financialControllerServices) {
					if(StringHelper.isBlank(financialControllerService.getLocalityCode()) && StringHelper.isNotBlank(sectionCode) 
							&& sectionCode.equals(financialControllerService.getSectionCode()) ) {
						for(ScopeFunction scopeFunction : scopeFunctions)
							if(scopeFunction.getFunctionAsString().equals(Function.CODE_FINANCIAL_CONTROLLER_HOLDER) 
									&& scopeFunction.getScopeIdentifier().equals(financialControllerService.getIdentifier())							 ) {
								return scopeFunction;
							}
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
						if(scopeFunction.getFunctionAsString().equals(Function.CODE_ACCOUNTING_HOLDER) && scopeFunction.getScopeIdentifier().equals(accountingService.getIdentifier()))
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
	
	@Transactional
	@Override
	public TransactionResult saveScopeFunctions(Collection<Assignments> collection) {
		return saveScopeFunctions(collection, null);
	}
	
	/**
	 * Enregistre les modifications.
	 */
	public static TransactionResult saveScopeFunctions(Collection<Assignments> collection,EntityManager entityManager) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("Assignments collection", collection);
		TransactionResult transactionResult = new TransactionResult().setName("Enregistrement").setTupleName("Affectation");
		collection.forEach(x -> {
			x.set__auditFunctionality__("Modification");	
		});
		setAssistants(collection,List.of(Assignments.FIELD_CREDIT_MANAGER_HOLDER,Assignments.FIELD_AUTHORIZING_OFFICER_HOLDER,Assignments.FIELD_FINANCIAL_CONTROLLER_HOLDER
				,Assignments.FIELD_ACCOUNTING_HOLDER),entityManager);
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments().setEntityManager(entityManager);
		queryExecutorArguments.setObjects(CollectionHelper.cast(Object.class, collection));
		EntityUpdater.getInstance().update(queryExecutorArguments);
		transactionResult.setNumberOfUpdateFromSavables(collection);
		transactionResult.log(AssignmentsBusinessImpl.class);
		String actorCode = collection.iterator().next().get__auditWho__();
		exportAsynchronously(actorCode);
		return transactionResult;
	}
	
	private static void setAssistants(Collection<Assignments> collection, Collection<String> overridablesHoldersFieldsNames,EntityManager entityManager) {
		if(CollectionHelper.isEmpty(collection))
			return;
		//Collection<Assignments> database = __persistence__.readBySystemIdentifiers(FieldHelper.readSystemIdentifiers(collection));
		
		Collection<Assignments> database = new ReaderByCollection.AbstractImpl<String, Assignments>() {
			@Override
			protected Collection<Assignments> __read__(Collection<String> identifiers) {
				return EntityFinder.getInstance().findMany(Assignments.class, identifiers); //__persistence__.readBySystemIdentifiers(CollectionHelper.cast(Object.class,identifiers));
			}
		}.read(FieldHelper.readSystemIdentifiersAsStrings(collection));
		
		collection.forEach(x -> {
			Assignments origin = CollectionHelper.isEmpty(database) ? null 
					: CollectionHelper.getFirst(database.stream().filter(y -> y.getIdentifier().equals(x.getIdentifier())).collect(Collectors.toList()));
			
			if(Boolean.TRUE.equals(CollectionHelper.contains(overridablesHoldersFieldsNames, Assignments.FIELD_CREDIT_MANAGER_HOLDER))) {				
				if(x.getCreditManagerHolder() == null) {
					x.setCreditManagerAssistant(null);
				}else {
					if(origin != null /*&& !x.getCreditManagerHolder().equals(origin.getCreditManagerHolder())*/) {
						x.setCreditManagerAssistant(CollectionHelper.getFirst(ScopeFunctionQuerier.getInstance()
								.readByParentsIdentifiers(List.of(x.getCreditManagerHolder().getIdentifier()))));
						if(x.getCreditManagerAssistant() == null) {
							String assistantCode = computeAssistantCodeFromHolderCode(x.getCreditManagerHolder().getCode(),1);
							if(StringHelper.isNotBlank(assistantCode)) {
								ScopeFunction assistant = CodeExecutor.getInstance().getOne(ScopeFunction.class, assistantCode); //__inject__(ScopeFunctionPersistence.class).readByBusinessIdentifier(assistantCode);
								if(assistant != null) {
									x.setCreditManagerAssistant(assistant);
								}
							}
						}					
					}
				}
			}
			
			if(Boolean.TRUE.equals(CollectionHelper.contains(overridablesHoldersFieldsNames, Assignments.FIELD_AUTHORIZING_OFFICER_HOLDER))) {
				if(x.getAuthorizingOfficerHolder() == null) {
					x.setAuthorizingOfficerAssistant(null);
				}else {
					if(origin != null /*&& !x.getAuthorizingOfficerHolder().equals(origin.getAuthorizingOfficerHolder())*/) {
						x.setAuthorizingOfficerAssistant(CollectionHelper.getFirst(ScopeFunctionQuerier.getInstance()
								.readByParentsIdentifiers(List.of(x.getAuthorizingOfficerHolder().getIdentifier()))));
						if(x.getAuthorizingOfficerAssistant() == null) {
							String assistantCode = computeAssistantCodeFromHolderCode(x.getAuthorizingOfficerHolder().getCode(),2);
							if(StringHelper.isNotBlank(assistantCode)) {
								ScopeFunction assistant = CodeExecutor.getInstance().getOne(ScopeFunction.class, assistantCode);
								if(assistant != null) {
									x.setAuthorizingOfficerAssistant(assistant);
								}
							}
						}						
					}
				}
			}
			
			if(Boolean.TRUE.equals(CollectionHelper.contains(overridablesHoldersFieldsNames, Assignments.FIELD_FINANCIAL_CONTROLLER_HOLDER))) {
				if(x.getFinancialControllerHolder() == null) {
					x.setFinancialControllerAssistant(null);
				}else {
					if(origin != null /*&& !x.getFinancialControllerHolder().equals(origin.getFinancialControllerHolder())*/) {
						x.setFinancialControllerAssistant(CollectionHelper.getFirst(ScopeFunctionQuerier.getInstance()
								.readByParentsIdentifiers(List.of(x.getFinancialControllerHolder().getIdentifier()))));
						if(x.getFinancialControllerAssistant() == null) {
							String assistantCode = computeAssistantCodeFromHolderCode(x.getFinancialControllerHolder().getCode(),3);
							if(StringHelper.isNotBlank(assistantCode)) {
								ScopeFunction assistant = CodeExecutor.getInstance().getOne(ScopeFunction.class, assistantCode);
								if(assistant != null) {
									x.setFinancialControllerAssistant(assistant);
								}
							}
						}
					}
				}
			}

			if(Boolean.TRUE.equals(CollectionHelper.contains(overridablesHoldersFieldsNames, Assignments.FIELD_ACCOUNTING_HOLDER))) {
				if(x.getAccountingHolder() == null) {
					x.setAccountingAssistant(null);
				}else {
					if(origin != null /*&& !x.getAccountingHolder().equals(origin.getAccountingHolder())*/) {
						x.setAccountingAssistant(CollectionHelper.getFirst(ScopeFunctionQuerier.getInstance()
								.readByParentsIdentifiers(List.of(x.getAccountingHolder().getIdentifier()))));
						if(x.getAccountingAssistant() == null) {
							String assistantCode = computeAssistantCodeFromHolderCode(x.getAccountingHolder().getCode(),4);
							if(StringHelper.isNotBlank(assistantCode)) {
								ScopeFunction assistant = CodeExecutor.getInstance().getOne(ScopeFunction.class, assistantCode);
								if(assistant != null) {
									x.setAccountingAssistant(assistant);
								}
							}
						}						
					}
				}
			}	
		});
	}
	
	@Transactional
	@Override
	public TransactionResult applyModel(Assignments model, Filter filter, Collection<String> overridablesFieldsNames,String actorCode) {		
		return applyModel(model, filter, overridablesFieldsNames, actorCode, null);
	}
	
	/**
	 * La liste ,éligible sur la base du filtre, est modifiée avec les valeurs du modèle.
	 * NB : Si une valeur est non nulle alors elle sera écrasée si cela à été explicitement spécifié.
	 */
	public static TransactionResult applyModel(Assignments model, Filter filter, Collection<String> overridablesFieldsNames,String actorCode,EntityManager entityManager) {
		ThrowableHelper.throwIllegalArgumentExceptionIfNull("model", model);
		ThrowableHelper.throwIllegalArgumentExceptionIfNull("filter", filter);
		TransactionResult transactionResult = new TransactionResult().setName("Application de modèle").setTupleName("Affectation").setIsTupleNameFeminine(Boolean.TRUE);
		LogHelper.logInfo(String.format("Modèle d'écrasement : %s|%s|%s|%s", model.getCreditManagerHolder(),model.getAuthorizingOfficerHolder()
				,model.getFinancialControllerHolder(),model.getAccountingHolder()), AssignmentsBusinessImpl.class);
		LogHelper.logInfo(String.format("Filtre d'écrasement : %s", filter), AssignmentsBusinessImpl.class);
		LogHelper.logInfo(String.format("Options d'écrasement : %s", overridablesFieldsNames), AssignmentsBusinessImpl.class);
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments();
		queryExecutorArguments.setFilter(filter);
		queryExecutorArguments.setQuery(new Query().setIdentifier(AssignmentsQuerier.QUERY_IDENTIFIER_COUNT_DYNAMIC));		
		LogHelper.logInfo(String.format("Compte des affectations à traiter en cours..."), AssignmentsBusinessImpl.class);
		Long t = System.currentTimeMillis();
		Long numberOfExecutionImputations = AssignmentsQuerier.getInstance().count(queryExecutorArguments);
		LogHelper.logInfo(String.format("%s affectations à traiter compté en %s", numberOfExecutionImputations,TimeHelper.formatDuration(System.currentTimeMillis() - t)), AssignmentsBusinessImpl.class);
		if(NumberHelper.isLessThanOrEqualZero(numberOfExecutionImputations))
			return null;
		Integer numberOfBatches = (int) (numberOfExecutionImputations / READ_BATCH_SIZE) + (numberOfExecutionImputations % READ_BATCH_SIZE == 0 ? 0 : 1);
		LogHelper.logInfo(String.format("taille du lot est de %s. %s lot(s) à traiter",READ_BATCH_SIZE,numberOfBatches), AssignmentsBusinessImpl.class);
		queryExecutorArguments.setQuery(new Query().setIdentifier(AssignmentsQuerier.QUERY_IDENTIFIER_READ_DYNAMIC)).addFlags(AssignmentsQuerier.FLAG_APPLY_MODEL)
			.setNumberOfTuples(READ_BATCH_SIZE);
		for(Integer index = 0; index < numberOfBatches; index = index + 1)
			applyModel(model, overridablesFieldsNames,actorCode, queryExecutorArguments.setFirstTupleIndex(index * READ_BATCH_SIZE),transactionResult,entityManager);	
		transactionResult.log(AssignmentsBusinessImpl.class);
		exportAsynchronously(actorCode);
		return transactionResult;
	}
	
	private static void applyModel(Assignments model, Collection<String> overridablesFieldsNames,String actorCode,QueryExecutorArguments queryExecutorArguments,TransactionResult transactionResult,EntityManager entityManager) {
		Long t = System.currentTimeMillis();
		Collection<Assignments> collection = AssignmentsQuerier.getInstance().readMany(queryExecutorArguments);	
		LogHelper.logInfo(String.format("\tChargement de %s affectation(s) à partir l'index %s en %s",CollectionHelper.getSize(collection)
				,queryExecutorArguments.getFirstTupleIndex(),TimeHelper.formatDuration(System.currentTimeMillis() - t)), AssignmentsBusinessImpl.class);
		if(CollectionHelper.isEmpty(collection))
			return;
		/*
		collection = collection.stream().filter(index -> Boolean.TRUE.equals(Assignments.isOneHolderHasChanged(index, model,overridablesFieldsNames))).collect(Collectors.toList());
		if(CollectionHelper.isEmpty(collection))
			return;
		LogHelper.logInfo(String.format("\t%s affectation(s) ayant un changement à prendre en compte",CollectionHelper.getSize(collection)), getClass());
		*/
		Collection<Assignments> changes = new ArrayList<>();
		collection.parallelStream().forEach(index -> {
			
			Boolean changed = null;
			if(index.getCreditManagerHolder() == null || CollectionHelper.contains(overridablesFieldsNames, Assignments.FIELD_CREDIT_MANAGER_HOLDER)) {			
				if(!Boolean.TRUE.equals(IdentifiableSystem.areIdentifiersEqual(index.getCreditManagerHolder(), model.getCreditManagerHolder())))
					changed = Boolean.TRUE;
				index.setCreditManagerHolder(model.getCreditManagerHolder());
			}
			
			if(index.getAuthorizingOfficerHolder() == null || CollectionHelper.contains(overridablesFieldsNames, Assignments.FIELD_AUTHORIZING_OFFICER_HOLDER)) {
				if(!Boolean.TRUE.equals(IdentifiableSystem.areIdentifiersEqual(index.getAuthorizingOfficerHolder(), model.getAuthorizingOfficerHolder())))
					changed = Boolean.TRUE;
				index.setAuthorizingOfficerHolder(model.getAuthorizingOfficerHolder());
			}
			
			if(index.getFinancialControllerHolder() == null || CollectionHelper.contains(overridablesFieldsNames, Assignments.FIELD_FINANCIAL_CONTROLLER_HOLDER)) {
				if(!Boolean.TRUE.equals(IdentifiableSystem.areIdentifiersEqual(index.getFinancialControllerHolder(), model.getFinancialControllerHolder())))
					changed = Boolean.TRUE;
				index.setFinancialControllerHolder(model.getFinancialControllerHolder());
			}
			
			if(index.getAccountingHolder() == null || CollectionHelper.contains(overridablesFieldsNames, Assignments.FIELD_ACCOUNTING_HOLDER)) {
				if(!Boolean.TRUE.equals(IdentifiableSystem.areIdentifiersEqual(index.getAccountingHolder(), model.getAccountingHolder())))
					changed = Boolean.TRUE;
				index.setAccountingHolder(model.getAccountingHolder());
			}
			
			if(Boolean.TRUE.equals(changed)) {
				index.set__auditWho__(actorCode);
				index.set__auditFunctionality__("Modification en masse");
				changes.add(index);
			}
		});
		
		collection = changes;
		LogHelper.logInfo(String.format("\t%s affectation(s) ayant un changement à prendre en compte",CollectionHelper.getSize(collection)), AssignmentsBusinessImpl.class);
		if(CollectionHelper.isEmpty(collection))
			return;
		
		setAssistants(collection,overridablesFieldsNames,entityManager);
		
		t = System.currentTimeMillis();
		
		QueryExecutorArguments updaterQueryExecutorArguments = new QueryExecutorArguments();
		updaterQueryExecutorArguments.addObjects(CollectionHelper.cast(Object.class, collection));
		updaterQueryExecutorArguments.setIsEntityManagerFlushable(Boolean.TRUE).setIsEntityManagerClearable(Boolean.TRUE).setIsEntityManagerClosable(Boolean.TRUE);
		EntityUpdater.getInstance().update(updaterQueryExecutorArguments);
		LogHelper.logInfo(String.format("\tEnregistrement en %s",TimeHelper.formatDuration(System.currentTimeMillis() - t)), AssignmentsBusinessImpl.class);
		transactionResult.incrementNumberOfUpdate(Integer.valueOf(collection.size()).longValue());
		collection.clear();
		collection = null;
	}

	@Override @Transactional
	public BusinessEntity<Assignments> deleteAll() {
		QueryExecutor.getInstance().executeUpdateOrDelete(new QueryExecutorArguments().setQuery(new Query().setValue("DELETE FROM Assignments")));
		return this;
	}

	@Transactional
	@Override
	public void clean(String actorCode) {
		actorCode = ValueHelper.defaultToIfBlank(actorCode, EntityLifeCycleListener.AbstractImpl.DEFAULT_USER_NAME);
		AssignmentsQuerier.getInstance().clean(actorCode, "effacement", EntityLifeCycleListener.Event.UPDATE.getValue(), new Date());
	}
	
	@Transactional
	@Override
	public void import_(String actorCode) {
		actorCode = ValueHelper.defaultToIfBlank(actorCode, EntityLifeCycleListener.AbstractImpl.DEFAULT_USER_NAME);
		AssignmentsQuerier.getInstance().import_(actorCode, "importation", EntityLifeCycleListener.Event.CREATE.getValue()
				,EntityLifeCycleListener.Event.UPDATE.getValue(), new Date());
	}
	
	@Transactional
	@Override
	public void importNews(String actorCode) {
		actorCode = ValueHelper.defaultToIfBlank(actorCode, EntityLifeCycleListener.AbstractImpl.DEFAULT_USER_NAME);
		AssignmentsQuerier.getInstance().importNews(actorCode, "importation", EntityLifeCycleListener.Event.CREATE.getValue(), new Date());
	}
	
	@Transactional
	@Override
	public void export(String actorCode) {
		export(actorCode, null);
	}
	
	public static void export(String actorCode,EntityManager entityManager) {
		actorCode = ValueHelper.defaultToIfBlank(actorCode, EntityLifeCycleListener.AbstractImpl.DEFAULT_USER_NAME);
		AssignmentsQuerier.getInstance().export(actorCode, "exportation", EntityLifeCycleListener.Event.UPDATE.getValue(), new Date());
	}
	
	private static void exportAsynchronously(String actorCode) {
		if(!Boolean.TRUE.equals(EXPORT))
			return;
		new Thread(new Runnable() {
			@Override
			public void run() {
				try {
					export(actorCode,null);
				} catch (Exception exception) {
					LogHelper.log(exception, AssignmentsBusinessImpl.class);
				}
			}		
		}).start();
	}
}