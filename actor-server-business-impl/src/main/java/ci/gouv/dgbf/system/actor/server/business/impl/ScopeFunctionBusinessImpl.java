package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.enterprise.context.ApplicationScoped;
import javax.persistence.EntityManager;
import javax.transaction.Transactional;

import org.apache.commons.lang3.StringUtils;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.log.LogHelper;
import org.cyk.utility.__kernel__.map.MapHelper;
import org.cyk.utility.__kernel__.number.NumberHelper;
import org.cyk.utility.__kernel__.object.__static__.persistence.EntityLifeCycleListener;
import org.cyk.utility.__kernel__.properties.Properties;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.throwable.ThrowableHelper;
import org.cyk.utility.__kernel__.throwable.ThrowablesMessages;
import org.cyk.utility.__kernel__.time.TimeHelper;
import org.cyk.utility.__kernel__.value.ValueHelper;
import org.cyk.utility.business.TransactionResult;
import org.cyk.utility.business.server.EntityCreator;
import org.cyk.utility.business.server.EntityUpdater;
import org.cyk.utility.business.server.NativeQueryStringExecutor;
import org.cyk.utility.persistence.EntityManagerGetter;
import org.cyk.utility.persistence.PersistenceHelper;
import org.cyk.utility.persistence.query.EntityFinder;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.server.query.executor.field.CodeExecutor;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;
import org.cyk.utility.server.business.BusinessEntity;
import org.cyk.utility.server.business.BusinessFunctionCreator;
import org.cyk.utility.server.business.BusinessFunctionModifier;
import org.cyk.utility.server.business.BusinessServiceProvider;

import ci.gouv.dgbf.system.actor.server.business.api.ScopeFunctionBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ScopeFunctionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.FunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeTypeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AccountingService;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AuthorizingOfficerService;
import ci.gouv.dgbf.system.actor.server.persistence.entities.FinancialControllerService;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Locality;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeTypeFunction;

@ApplicationScoped
public class ScopeFunctionBusinessImpl extends AbstractBusinessEntityImpl<ScopeFunction, ScopeFunctionPersistence> implements ScopeFunctionBusiness,Serializable {
	private static final long serialVersionUID = 1L;
	
	public static TransactionResult createByScopeIdentifierByCategoryCode(String scopeIdentifier,String categoryCode,String name,String actorCode,Boolean throwOnExisting,EntityManager entityManager) {
		ThrowableHelper.throwIllegalArgumentExceptionIfBlank("identifiant domaine", scopeIdentifier);
		ThrowableHelper.throwIllegalArgumentExceptionIfBlank("code categorie", categoryCode);
		ThrowableHelper.throwIllegalArgumentExceptionIfBlank("nom d'utilisateur", actorCode);
		String scopeTypeCode = ScopeFunction.getScopeTypeCodeFromCategoryCode(categoryCode);
		ThrowableHelper.throwIllegalArgumentExceptionIfNull("type de domaine", scopeTypeCode);
		Scope scope = EntityFinder.getInstance().find(Scope.class, scopeIdentifier);
		if(scope == null)
			throw new RuntimeException(String.format("Le domaine <<%s>> n'existe pas", scopeIdentifier));
		String functionCode = ScopeType.getHolderFunctionCode(scopeTypeCode);
		ThrowableHelper.throwIllegalArgumentExceptionIfBlank("code fonction", functionCode);
		Function function = CodeExecutor.getInstance().getOne(Function.class, functionCode);
		if(function == null)
			throw new RuntimeException(String.format("La fonction <<%s>> n'existe pas", functionCode));
		if(Boolean.TRUE.equals(throwOnExisting)) {
			Long count = ScopeFunctionQuerier.getInstance().countByScopeIdentifierByFunctionCode(scope.getIdentifier(), functionCode);
			if(NumberHelper.isGreaterThanZero(count))
				throw new RuntimeException(String.format("Le domaine <<%s>> a déja %s <<%s>>.", scope,count,functionCode));	
		}
		ScopeFunction scopeFunction = new ScopeFunction().setScope(scope).setFunction(function).setCodePrefix(categoryCode).setName(name);		
		scopeFunction.set__auditWho__(actorCode);
		TransactionResult transactionResult = new TransactionResult().setName(String.format("création de %s | %s , %s",function.getName(),scopeTypeCode,scopeIdentifier))
				.setTupleName(function.getName());
		create(List.of(scopeFunction), entityManager);
		transactionResult.incrementNumberOfCreation(2l);
		transactionResult.log(ScopeFunctionBusinessImpl.class);
		return transactionResult;
	}
	
	@Override @Transactional
	public TransactionResult createByScopeIdentifierByCategoryCode(String scopeIdentifier,String categoryCode,String name,String actorCode,Boolean throwOnExisting) {
		TransactionResult result = createByScopeIdentifierByCategoryCode(scopeIdentifier,categoryCode,name,actorCode,throwOnExisting,EntityManagerGetter.getInstance().get());
		exportAsynchronously(actorCode);
		return result;		
	}
	
	public static void create(Collection<ScopeFunction> scopeFunctions, EntityManager entityManager) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("scopeFunctions", scopeFunctions);		
		ThrowablesMessages throwablesMessages = new ThrowablesMessages();
		for(ScopeFunction scopeFunction : scopeFunctions) {
			if(StringHelper.isNotBlank(scopeFunction.getParentIdentifier()))
				continue;
			if(scopeFunction.getScope() == null)
				throwablesMessages.add("Le domaine est obligatoire");
			if(scopeFunction.getFunction() == null)
				throwablesMessages.add("La fonction est obligatoire");
		}
		throwablesMessages.throwIfNotEmpty();
		//add assistants where possible
		Collection<ScopeFunction> allScopeFunctions = new ArrayList<>(scopeFunctions);
		for(ScopeFunction scopeFunction : scopeFunctions) {
			if(scopeFunction.getScope() == null && StringHelper.isNotBlank(scopeFunction.getParentIdentifier())) {
				if(scopeFunction.getParent() == null)
					scopeFunction.setParent(EntityFinder.getInstance().find(ScopeFunction.class, scopeFunction.getParentIdentifier()));
				if(scopeFunction.getParent() != null)
					scopeFunction.setScope(scopeFunction.getParent().getScope());
			}
			if(scopeFunction.getFunction() == null && StringHelper.isNotBlank(scopeFunction.getParentIdentifier())) {
				if(scopeFunction.getParent() == null)
					scopeFunction.setParent(EntityFinder.getInstance().find(ScopeFunction.class, scopeFunction.getParentIdentifier()));
				if(scopeFunction.getParent() != null)
					scopeFunction.setFunctionFromIdentifier(Function.formatAssistantIdentifier(scopeFunction.getParent().getFunction().getIdentifier()));
			}
			if(scopeFunction.getParent() == null && StringHelper.isNotBlank(scopeFunction.getParentIdentifier())) {
				scopeFunction.setParent(EntityFinder.getInstance().find(ScopeFunction.class, scopeFunction.getParentIdentifier()));
			}
			if(Boolean.TRUE.equals(scopeFunction.getFunction().isCodeBelongsToExecutionHoldersCodes())) {
				Function assistant = FunctionQuerier.getInstance().readByCode(Function.formatAssistantCode(scopeFunction.getFunction().getCode()));
				if(assistant == null)
					continue;
				if(allScopeFunctions == null)
					allScopeFunctions = new ArrayList<>();
				ScopeFunction assistantScopeFunction = new ScopeFunction().setScope(scopeFunction.getScope()).setFunction(assistant).setShared(Boolean.TRUE)
						.setParent(scopeFunction)
						.setCodePrefix(ScopeFunction.getAssistantCategoryCodeFromHolderCategoryCode(scopeFunction.getCodePrefix()))
						.setName(scopeFunction.getName());
				assistantScopeFunction
					.set__auditFunctionality__(scopeFunction.get__auditFunctionality__())
					.set__auditWhat__(scopeFunction.get__auditWhat__())
					.set__auditWho__(scopeFunction.get__auditWho__());
				if(StringHelper.isNotBlank(scopeFunction.getName()))
					assistantScopeFunction.setName("Assistant "+scopeFunction.getName());
				
				allScopeFunctions.add(assistantScopeFunction);
			}else if(Boolean.TRUE.equals(scopeFunction.getFunction().isCodeBelongsToExecutionAssisantsCodes())) {
				if(StringHelper.isNotBlank(scopeFunction.getParentIdentifier())) {
					scopeFunction.setParent(EntityFinder.getInstance().find(ScopeFunction.class, scopeFunction.getParentIdentifier()));
					if(scopeFunction.getParent() != null) {
						scopeFunction.getParent().setChildrenCount(NumberHelper.get(Byte.class,
								ScopeFunctionQuerier.getInstance().countByParentsIdentifiers(List.of(scopeFunction.getParent().getIdentifier()))));
					}
				}
			}
		}
		allScopeFunctions.forEach(scopeFunction -> {
			__listenExecuteCreateOrUpdateBefore__(scopeFunction);
			scopeFunction.setIdentifier(scopeFunction.getCode());
		});
		Collection<String> existingCodes = CodeExecutor.getInstance().getExisting(ScopeFunction.class, FieldHelper.readBusinessIdentifiersAsStrings(scopeFunctions));
		if(CollectionHelper.isNotEmpty(existingCodes))
			throw new RuntimeException("Les codes suivants existe déjà : "+existingCodes);
		
		org.cyk.utility.persistence.query.EntityCreator.getInstance().createMany(allScopeFunctions,entityManager);
		//__persistence__.createMany(allScopeFunctions);
		for(ScopeFunction scopeFunction : allScopeFunctions)
			if(scopeFunction.getParent() != null) {
				scopeFunction.setParentIdentifier(scopeFunction.getParent().getIdentifier());
				org.cyk.utility.persistence.query.EntityUpdater.getInstance().updateMany(entityManager, scopeFunction);
				//__persistence__.update(scopeFunction);
			}
	}
	
	/**/
	
	@Override
	protected void __listenExecuteCreateBefore__(ScopeFunction scopeFunction, Properties properties,BusinessFunctionCreator function) {
		super.__listenExecuteCreateBefore__(scopeFunction, properties, function);
		__listenExecuteCreateOrUpdateBefore__(scopeFunction);
	}
	
	@Override
	protected void __listenExecuteCreateAfter__(ScopeFunction scopeFunction, Properties properties,BusinessFunctionCreator function) {
		super.__listenExecuteCreateAfter__(scopeFunction, properties, function);
	}
	
	@Override
	protected void __listenExecuteUpdateBefore__(ScopeFunction scopeFunction, Properties properties,BusinessFunctionModifier function) {
		super.__listenExecuteUpdateBefore__(scopeFunction, properties, function);
		__listenExecuteCreateOrUpdateBefore__(scopeFunction);
	}
	
	private static void __listenExecuteCreateOrUpdateBefore__(ScopeFunction scopeFunction) {
		scopeFunction.setNumberOfActor(ScopeFunctionPersistence.computeNumberOfActor(scopeFunction.getShared()));
		if(StringHelper.isBlank(scopeFunction.getIdentifier()) /*&& StringHelper.isBlank(scopeFunction.getCode()) && StringHelper.isBlank(scopeFunction.getName())*/) {
			__codify__(List.of(scopeFunction));
			if(StringHelper.isBlank(scopeFunction.getCode()))
				throw new RuntimeException(String.format("Impossible de générer le code à partir du domaine %s et la fonction %s",scopeFunction.getScope()
						,scopeFunction.getFunction()));
			if(scopeFunction.getFunction().isCodeBelongsToExecutionHoldersCodes()) {
				Long count = EntityManagerGetter.getInstance().get().createQuery(String.format("SELECT COUNT(p) FROM ScopeFunction p WHERE p.%1$s = :%1$s",ScopeFunction.FIELD_CODE), Long.class)
						.setParameter(ScopeFunction.FIELD_CODE, scopeFunction.getCode()).getSingleResult();
				if(NumberHelper.isGreaterThanOrEqualOne(count))
					throw new RuntimeException(String.format("%s existe déja",scopeFunction.getCode()));				
			}else {
				Integer retry = 0;
				do {
					Long count = EntityManagerGetter.getInstance().get().createQuery(String.format("SELECT COUNT(p) FROM ScopeFunction p WHERE p.%1$s = :%1$s",ScopeFunction.FIELD_CODE), Long.class)
						.setParameter(ScopeFunction.FIELD_CODE, scopeFunction.getCode()).getSingleResult();
					if(NumberHelper.isEqualToZero(count) || retry > 3)
						break;					
					LogHelper.logInfo(String.format("Code %s exist. going to regenerate. %s", scopeFunction.getCode(),retry), ScopeBusinessImpl.class);				
					scopeFunction.setCode(StringUtils.substring(scopeFunction.getCode(), 0, 7)+(NumberHelper.getInteger(StringUtils.substring(scopeFunction.getCode(), 7))+1));
					if(scopeFunction.getCode().length() > 8)
						throw new RuntimeException(String.format("Impossible de regénérer le code à partir du domaine %s et la fonction %s",scopeFunction.getScope()
								,scopeFunction.getFunction()));
					retry++;
				}while(true);
			}
					
			Long count = EntityManagerGetter.getInstance().get().createQuery(String.format("SELECT COUNT(p) FROM ScopeFunction p WHERE p.%1$s = :%1$s AND p.%2$s = :%2$s"
					,ScopeFunction.FIELD_FUNCTION,ScopeFunction.FIELD_DOCUMENT_NUMBER), Long.class)
					.setParameter(ScopeFunction.FIELD_FUNCTION, scopeFunction.getFunction())
					.setParameter(ScopeFunction.FIELD_DOCUMENT_NUMBER, scopeFunction.getDocumentNumber())
					.getSingleResult();
			if(NumberHelper.isGreaterThanOrEqualOne(count))
				throw new RuntimeException(String.format("%s , Le couple (fonction = %s,numéro de document = %s) existe déja",scopeFunction.getCode()
						,scopeFunction.getFunction().getCode(),scopeFunction.getDocumentNumber()));
		}
	}
	
	@Override @Transactional
	public BusinessServiceProvider<ScopeFunction> createMany(Collection<ScopeFunction> scopeFunctions, Properties properties) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("scopeFunctions", scopeFunctions);		
		ThrowablesMessages throwablesMessages = new ThrowablesMessages();
		for(ScopeFunction scopeFunction : scopeFunctions) {
			if(StringHelper.isNotBlank(scopeFunction.getParentIdentifier()))
				continue;
			if(scopeFunction.getScope() == null)
				throwablesMessages.add("Le domaine est obligatoire");
			if(scopeFunction.getFunction() == null)
				throwablesMessages.add("La fonction est obligatoire");
		}
		throwablesMessages.throwIfNotEmpty();
		//add assistants where possible
		Collection<ScopeFunction> allScopeFunctions = new ArrayList<>(scopeFunctions);
		for(ScopeFunction scopeFunction : scopeFunctions) {
			if(scopeFunction.getScope() == null && StringHelper.isNotBlank(scopeFunction.getParentIdentifier())) {
				if(scopeFunction.getParent() == null)
					scopeFunction.setParent(EntityFinder.getInstance().find(ScopeFunction.class, scopeFunction.getParentIdentifier()));
				if(scopeFunction.getParent() != null)
					scopeFunction.setScope(scopeFunction.getParent().getScope());
			}
			if(scopeFunction.getFunction() == null && StringHelper.isNotBlank(scopeFunction.getParentIdentifier())) {
				if(scopeFunction.getParent() == null)
					scopeFunction.setParent(EntityFinder.getInstance().find(ScopeFunction.class, scopeFunction.getParentIdentifier()));
				if(scopeFunction.getParent() != null)
					scopeFunction.setFunctionFromIdentifier(Function.formatAssistantIdentifier(scopeFunction.getParent().getFunction().getIdentifier()));
			}
			if(scopeFunction.getParent() == null && StringHelper.isNotBlank(scopeFunction.getParentIdentifier())) {
				scopeFunction.setParent(EntityFinder.getInstance().find(ScopeFunction.class, scopeFunction.getParentIdentifier()));
			}
			if(Boolean.TRUE.equals(scopeFunction.getFunction().isCodeBelongsToExecutionHoldersCodes())) {
				Function assistant = FunctionQuerier.getInstance().readByCode(Function.formatAssistantCode(scopeFunction.getFunction().getCode()));
				if(assistant == null)
					continue;
				if(allScopeFunctions == null)
					allScopeFunctions = new ArrayList<>();
				ScopeFunction assistantScopeFunction = new ScopeFunction().setScope(scopeFunction.getScope()).setFunction(assistant).setShared(Boolean.TRUE)
						.setParent(scopeFunction)
						.setCodePrefix(ScopeFunction.getAssistantCategoryCodeFromHolderCategoryCode(scopeFunction.getCodePrefix()))
						.setName(scopeFunction.getName());
				if(StringHelper.isNotBlank(scopeFunction.getName()))
					assistantScopeFunction.setName("Assistant "+scopeFunction.getName());
				
				allScopeFunctions.add(assistantScopeFunction);
			}else if(Boolean.TRUE.equals(scopeFunction.getFunction().isCodeBelongsToExecutionAssisantsCodes())) {
				if(StringHelper.isNotBlank(scopeFunction.getParentIdentifier())) {
					scopeFunction.setParent(__persistence__.readBySystemIdentifier(scopeFunction.getParentIdentifier()));
					if(scopeFunction.getParent() != null) {
						scopeFunction.getParent().setChildrenCount(NumberHelper.get(Byte.class,
								ScopeFunctionQuerier.getInstance().countByParentsIdentifiers(List.of(scopeFunction.getParent().getIdentifier()))));
					}
				}				
			}
		}
		allScopeFunctions.forEach(scopeFunction -> {
			__listenExecuteCreateOrUpdateBefore__(scopeFunction);
			scopeFunction.setIdentifier(scopeFunction.getCode());
		});
		Collection<String> existingCodes = CodeExecutor.getInstance().getExisting(ScopeFunction.class, FieldHelper.readBusinessIdentifiersAsStrings(scopeFunctions));
		if(CollectionHelper.isNotEmpty(existingCodes))
			throw new RuntimeException("Les codes suivants existe déjà : "+existingCodes);
		EntityManager entityManager = EntityManagerGetter.getInstance().get();
		org.cyk.utility.persistence.query.EntityCreator.getInstance().createMany(allScopeFunctions,entityManager);
		//__persistence__.createMany(allScopeFunctions);
		for(ScopeFunction scopeFunction : allScopeFunctions)
			if(scopeFunction.getParent() != null) {
				scopeFunction.setParentIdentifier(scopeFunction.getParent().getIdentifier());
				org.cyk.utility.persistence.query.EntityUpdater.getInstance().updateMany(entityManager, scopeFunction);
				//__persistence__.update(scopeFunction);
			}
		//throwablesMessages.throwIfNotEmpty();
		return this;
	}
	
	@Override
	public BusinessServiceProvider<ScopeFunction> saveMany(Collection<ScopeFunction> scopeFunctions, Properties properties) {
		super.saveMany(scopeFunctions, properties);
		exportAsynchronously(scopeFunctions.iterator().next().get__auditWho__());
		return this;
	}
	
	@Override
	public void deriveByFunctions(Collection<Function> functions) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("functions", functions);
		Long t0 = System.currentTimeMillis();
		LogHelper.logInfo(String.format("Dérivation des postes des fonctions %s",functions), getClass());		
		Collection<ScopeTypeFunction> scopeTypeFunctions = ScopeTypeFunctionQuerier.getInstance().readByFunctionsIdentifiers(FieldHelper.readSystemIdentifiersAsStrings(functions));
		
		for(Function function : functions) {
			LogHelper.logInfo(String.format("\tTraitement de la fonction %s",function), getClass());
			Collection<ScopeTypeFunction> scopeTypeFunctionsOfFunction = scopeTypeFunctions.stream()
					.filter(scopeTypeFunction -> scopeTypeFunction.getFunction().equals(function) && Boolean.TRUE.equals(scopeTypeFunction.getScopeFunctionDerivable())).collect(Collectors.toList()); 
			LogHelper.logInfo(String.format("\tNombre d'association(s) avec type de domaine où l'option dérivable est vrai : %s",scopeTypeFunctionsOfFunction.size()), getClass());
			if(CollectionHelper.isEmpty(scopeTypeFunctionsOfFunction))
				continue;		
			for(ScopeTypeFunction scopeTypeFunction : scopeTypeFunctionsOfFunction) {
				LogHelper.logInfo(String.format("\t\tChargement des %s n'ayant pas de %s",scopeTypeFunction.getScopeType().getCode(),scopeTypeFunction.getFunction().getCode()), getClass());
				Collection<Scope> scopes = ScopeQuerier.getInstance().readWhereFunctionDoesNotExistByTypesIdentifiersByFunctionsIdentifiers(
						List.of(scopeTypeFunction.getScopeType().getIdentifier()),List.of(scopeTypeFunction.getFunction().getIdentifier()));
				LogHelper.logInfo(String.format("\t\t%s chargée(s)",CollectionHelper.getSize(scopes)), getClass());
				if(CollectionHelper.isEmpty(scopes))
					continue;
				LogHelper.logInfo(String.format("\t\tInstantiation des postes"), getClass());
				Collection<AuthorizingOfficerService> authorizingOfficerServices = null;
				if(scopeTypeFunction.getScopeType().getCode().equals(ScopeType.CODE_SERVICE_ORD)) {
					//prepare localities
					authorizingOfficerServices = new ArrayList<>();
					List<List<String>> batches = CollectionHelper.getBatches(scopes.stream().map(x -> x.getIdentifier()).collect(Collectors.toList()), 900);
					for(List<String> identifiers : batches) {
						CollectionHelper.add(authorizingOfficerServices, Boolean.TRUE, EntityFinder.getInstance().findMany(AuthorizingOfficerService.class, identifiers));
					}			
				}
				Collection<ScopeFunction> scopeFunctions = new ArrayList<>();
				for(Scope scope : scopes) {
					Locality locality = null;
					if(scope.getType().getCode().equals(ScopeType.CODE_SERVICE_ORD)) {
						AuthorizingOfficerService authorizingOfficerService = EntityFinder.getInstance().find(AuthorizingOfficerService.class, scope.getIdentifier());
						if(authorizingOfficerService != null)
							locality = authorizingOfficerService.getLocality();
					}
					//if(locality == null)
					//	locality = LocalityQuerier.getInstance().readByCode(Locality.CODE_SOUS_PREFECTURE_BINGERVILLE);
					ScopeFunction scopeFunction = new ScopeFunction().setScope(scope).setFunction(function).setLocality(locality).setNumberOfActor(function.getNumberOfActorPerScope());
					scopeFunctions.add(scopeFunction);
				}
				LogHelper.logInfo(String.format("\t\t%s poste(s) instantié(s)",CollectionHelper.getSize(scopeFunctions)), getClass());
				if(CollectionHelper.isEmpty(scopeFunctions))
					return;
				LogHelper.logInfo(String.format("\t\tCodification des postes",scopeFunctions.size()), getClass());
				__codify__(scopeTypeFunction.getScopeType().getCode(),function.getCode(),null,scopeFunctions,scopeTypeFunction.getScopeFunctionCodeScript()
						,scopeTypeFunction.getScopeFunctionNameScript());
				LogHelper.logInfo(String.format("\t\tEnregistrement des postes",scopeFunctions.size()), getClass());
				EntityCreator.getInstance().createMany(CollectionHelper.cast(Object.class, scopeFunctions));
			}
		}
		Long duration = System.currentTimeMillis() - t0;
		LogHelper.logInfo(String.format("Dérivation terminée en %s",TimeHelper.formatDuration(duration)), getClass());
	}
	
	@Override
	public void deriveByFunctionsIdentifiers(Collection<String> functionsIdentifiers) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("functionsIdentifiers", functionsIdentifiers);
		deriveByFunctions(EntityFinder.getInstance().findMany(Function.class, functionsIdentifiers));
	}
	
	protected Collection<Function> getHoldersAndAssistantsByHoldersFunctionsIdentifiers(Collection<String> holdersFunctionsIdentifiers) {
		Collection<Function> functions = EntityFinder.getInstance().findMany(Function.class, holdersFunctionsIdentifiers);
		if(CollectionHelper.isNotEmpty(functions)) {
			for(Function function : functions) {
				Function assistantFunction = FunctionQuerier.getInstance().readByCode(Function.formatAssistantCode(function.getCode()));
				if(assistantFunction == null)
					continue;
				functions.add(function);
			}
		}
		return functions;
	}
	
	@Override
	public void deriveHoldersAndAssistantsByHoldersFunctionsIdentifiers(Collection<String> holdersFunctionsIdentifiers) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("holdersFunctionsIdentifiers", holdersFunctionsIdentifiers);
		deriveByFunctions(getHoldersAndAssistantsByHoldersFunctionsIdentifiers(holdersFunctionsIdentifiers));
	}
	
	@Override
	public void deriveAll() {
		Collection<Function> functions = FunctionQuerier.getInstance().readWhereAssociatedToScopeType();
		if(CollectionHelper.isEmpty(functions))
			return;
		deriveByFunctions(functions);
	}
	
	/*
	@Override
	public void deriveAll() {
		LogHelper.logInfo(String.format("Dérivation de tous les postes en cours"), getClass());
		Long t0 = System.currentTimeMillis();
		LogHelper.logInfo("Chargement des association(s) de type de domaine et de fonction avec l'option dérivable à vrai", getClass());
		Collection<ScopeTypeFunction> scopeTypeFunctions = ScopeTypeFunctionQuerier.getInstance().readWhereScopeFunctionDerivableIsTrue();
		LogHelper.logInfo(String.format(CollectionHelper.getSize(scopeTypeFunctions)+" chargée(s)"), getClass());
		if(CollectionHelper.isEmpty(scopeTypeFunctions))
			return;
		LogHelper.logInfo("Détermination des type(s) de domaine(s)", getClass());
		Collection<ScopeType> scopeTypes = scopeTypeFunctions.stream().map(x -> x.getScopeType()).collect(Collectors.toSet());
		LogHelper.logInfo(String.format("%s déterminée(s) : %s",CollectionHelper.getSize(scopeTypes),scopeTypes), getClass());
		if(CollectionHelper.isEmpty(scopeTypes))
			return;		
		Collection<ScopeFunction> scopeFunctions = null;
		LogHelper.logInfo(String.format("Instantiation des postes par type de domaine en cours"), getClass());
		for(ScopeType scopeType : scopeTypes) {
			LogHelper.logInfo(String.format("\tChargement des %s n'ayant pas de poste",scopeType.getCode()), getClass());
			Collection<Scope> scopes = ScopeQuerier.getInstance().readWhereFunctionDoesNotExistByTypesIdentifiers(List.of(scopeType.getIdentifier()));
			LogHelper.logInfo(String.format("\t%s chargée(s)",CollectionHelper.getSize(scopes)), getClass());
			if(CollectionHelper.isEmpty(scopes))
				continue;
			LogHelper.logInfo(String.format("\tDétermination des fonctions"), getClass());
			Collection<Function> functions = scopeTypeFunctions.stream().filter(x -> x.getScopeType().equals(scopeType)).map(x -> x.getFunction()).collect(Collectors.toSet());
			LogHelper.logInfo(String.format("\t%s fonction(s) déterminée(s) : %s",CollectionHelper.getSize(functions),functions), getClass());
			if(CollectionHelper.isEmpty(functions))
				continue;
			for(Function function : functions)	{
				LogHelper.logInfo(String.format("\tInstantiation de %s poste(s) %s",scopes.size(),function.getCode()+" - "+function.getName()), getClass());
				for(Scope scope : scopes) {
					if(scopeFunctions == null)
						scopeFunctions = new ArrayList<>();
					ScopeFunction scopeFunction = new ScopeFunction().setScope(scope).setFunction(function);
					scopeFunction.setNumberOfActor(function.getNumberOfActorPerScope());
					scopeFunctions.add(scopeFunction);
				}
			}
		}
		LogHelper.logInfo(String.format("%s poste(s) instantié(s)",CollectionHelper.getSize(scopeFunctions)), getClass());
		if(CollectionHelper.isEmpty(scopeFunctions))
			return;
		LogHelper.logInfo(String.format("Codification de %s poste(s) en cours",scopeFunctions.size()), getClass());
		__codify__(scopeFunctions);
		LogHelper.logInfo(String.format("Enregistrement de %s poste(s) en cours",scopeFunctions.size()), getClass());
		EntityCreator.getInstance().createMany(CollectionHelper.cast(Object.class, scopeFunctions));
		Long duration = System.currentTimeMillis() - t0;
		LogHelper.logInfo(String.format("Dérivation %s poste(s) terminée en %s",scopeFunctions.size(),TimeHelper.formatDuration(duration)), getClass());
	}
	*/
	
	private static void __codify__(String scopeTypeCode,String functionCode,String codePrefix,Collection<ScopeFunction> pScopeFunctions,String codeScript,String nameScript) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("scopeFunctions", pScopeFunctions);
		Collection<ScopeFunction> scopeFunctions = pScopeFunctions.stream().filter(x -> x.getCodificationDate() == null).collect(Collectors.toList());
		if(CollectionHelper.isEmpty(scopeFunctions))
			return;
		Long count = ScopeFunctionQuerier.getInstance().countByFunctionsCodes(functionCode);
		//ScopeFunction scopeFunctionMax = StringHelper.isBlank(codePrefix) ? null : ScopeFunctionQuerier.getInstance().readMaxCodeWhereCodeStartsWith(codePrefix);
		//String categoryType = StringUtils.substring(codePrefix,0,1);
		//ScopeFunction scopeFunctionMax = StringHelper.isBlank(codePrefix) ? null : ScopeFunctionQuerier.getInstance().readMaxCodeUsingSubstringWhereCodeStartsWith(categoryType);
		Integer	orderNumber = NumberHelper.isEqualToZero(count) ? 0 : NumberHelper.getInteger(NumberHelper.add(ScopeFunctionQuerier.getInstance().readMaxOrderNumberByFunctionCode(functionCode),1));		
		Integer documentNumber = NumberHelper.isEqualToZero(count) ? null : NumberHelper.getInteger(NumberHelper.add(ScopeFunctionQuerier.getInstance().readMaxDocumentNumber(),1));
		LogHelper.logInfo(String.format("%s|%s|%s Numéro d'ordre à partir de %s , numéro de document à partir de %s", scopeTypeCode,functionCode,codePrefix
				,orderNumber,documentNumber), ScopeFunctionBusinessImpl.class);
		
		if(ScopeType.CODE_UA.equals(scopeTypeCode)) {
			if(documentNumber == null)
				documentNumber = 10000;
		}else if(ScopeType.CODE_SERVICE_ORD.equals(scopeTypeCode) || ScopeType.CODE_USB.equals(scopeTypeCode)) {
			if(documentNumber == null)
				documentNumber = 40000;
			if(ScopeType.CODE_SERVICE_ORD.equals(scopeTypeCode)) {
				List<String> authorizingOfficerServiceIdentifiers = scopeFunctions.stream().map(x -> x.getScope().getIdentifier()).collect(Collectors.toList());
				List<List<String>> batches = CollectionHelper.getBatches(authorizingOfficerServiceIdentifiers, 900);
				for(List<String> identifiers : batches) {
					Collection<AuthorizingOfficerService> authorizingOfficerServices = EntityFinder.getInstance().findMany(AuthorizingOfficerService.class, identifiers);
					for(ScopeFunction scopeFunction : scopeFunctions) {
						for(AuthorizingOfficerService authorizingOfficerService : authorizingOfficerServices)
							if(scopeFunction.getScope().getIdentifier().equals(authorizingOfficerService.getIdentifier())) {
								scopeFunction.setBudgetSpecializationUnit(authorizingOfficerService.getBudgetSpecializationUnit());
								scopeFunction.setLocality(authorizingOfficerService.getLocality());
								break;
							}
					}
				}
			}
		}else if(ScopeType.CODE_SERVICE_CF.equals(scopeTypeCode)) {
			if(documentNumber == null)
				documentNumber = 60000;
			List<String> financialControllerServiceIdentifiers = scopeFunctions.stream().map(x -> x.getScope().getIdentifier()).collect(Collectors.toList());
			List<List<String>> batches = CollectionHelper.getBatches(financialControllerServiceIdentifiers, 900);
			for(List<String> identifiers : batches) {
				Collection<FinancialControllerService> financialControllerServices = EntityFinder.getInstance().findMany(FinancialControllerService.class, identifiers);
				for(ScopeFunction scopeFunction : scopeFunctions) {
					for(FinancialControllerService financialControllerService : financialControllerServices)
						if(scopeFunction.getScope().getIdentifier().equals(financialControllerService.getIdentifier())) {
							scopeFunction.setLocality(financialControllerService.getLocality());
							scopeFunction.setActivityIdentifier(financialControllerService.getActivityIdentifier());
							break;
						}
				}
			}			
		}else if(ScopeType.CODE_SERVICE_CPT.equals(scopeTypeCode)) {
			if(documentNumber == null)
				documentNumber = 70000;
			List<String> accountingServiceIdentifiers = scopeFunctions.stream().map(x -> x.getScope().getIdentifier()).collect(Collectors.toList());
			List<List<String>> batches = CollectionHelper.getBatches(accountingServiceIdentifiers, 900);
			for(List<String> identifiers : batches) {
				Collection<AccountingService> accountingServices = EntityFinder.getInstance().findMany(AccountingService.class, identifiers);
				for(ScopeFunction scopeFunction : scopeFunctions) {
					for(AccountingService accountingService : accountingServices)
						if(scopeFunction.getScope().getIdentifier().equals(accountingService.getIdentifier())) {
							scopeFunction.setLocality(accountingService.getLocality());
							break;
						}
				}
			}			
		}
		LogHelper.logInfo(String.format("Numéro document à partir de %s", documentNumber), ScopeFunctionBusinessImpl.class);
		LocalDateTime codificationDate = LocalDateTime.now();
		for(ScopeFunction scopeFunction : scopeFunctions) {
			scopeFunction.setOrderNumber(orderNumber++);
			scopeFunction.setDocumentNumber(documentNumber++);
			scopeFunction.setCodificationDate(codificationDate);
		}
		ScopeFunction.computeCodeAndName(scopeTypeCode,scopeFunctions,orderNumber-scopeFunctions.size(),0, codeScript, nameScript);
		//find duplicates by code
		Map<String,Collection<ScopeFunction>> map = new HashMap<>();		
		for(String code : scopeFunctions.stream().map(x -> x.getCode()).collect(Collectors.toSet())) {
			Collection<ScopeFunction> value = scopeFunctions.stream().filter(x -> x.getCode().equals(code)).collect(Collectors.toList());
			if(value.size() > 1)
				map.put(code, value);
		}
		if(MapHelper.isNotEmpty(map)) {
			LogHelper.logInfo(String.format("%s duplicate(s) found",map.size()), ScopeFunctionBusinessImpl.class);
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.append("Doublons trouvés\r\n");
			for(Map.Entry<String,Collection<ScopeFunction>> entry : map.entrySet())
				stringBuilder.append(entry.getKey()+"\r\n");
			throw new RuntimeException(stringBuilder.toString());
		}
	}
	
	private static void __codify__(Collection<ScopeFunction> scopeFunctions) {
		if(CollectionHelper.isEmpty(scopeFunctions))
			return;
		Collection<ScopeType> scopeTypes = scopeFunctions.stream().map(x -> x.getScope().getType()).collect(Collectors.toList());
		if(CollectionHelper.isEmpty(scopeTypes))
			return;
		Collection<ScopeTypeFunction> scopeTypeFunctions = ScopeTypeFunctionQuerier.getInstance().readByScopeTypesCodes(scopeTypes.stream().map(x -> x.getCode())
				.collect(Collectors.toList()));
		if(CollectionHelper.isEmpty(scopeTypeFunctions))
			return;
		for(ScopeType scopeType : scopeTypes) {
			Collection<Function> functions = scopeFunctions.stream().filter(x -> x.getScope().getType().equals(scopeType))
					.map(x -> x.getFunction()).collect(Collectors.toList());
			for(Function function : functions) {
				ScopeTypeFunction scopeTypeFunction = CollectionHelper.getFirst(scopeTypeFunctions.stream()
						.filter(x -> x.getScopeType().equals(scopeType) && x.getFunction().equals(function))
						.collect(Collectors.toList()));
				Collection<ScopeFunction> collection = scopeFunctions.stream()
						.filter(x -> x.getScope().getType().equals(scopeType) && x.getFunction().equals(function))
						.collect(Collectors.toList());
				__codify__(scopeType.getCode(),function.getCode(),collection.iterator().next().getCodePrefix(), collection, scopeTypeFunction.getScopeFunctionCodeScript()
						, scopeTypeFunction.getScopeFunctionNameScript());
			}			
		}
	}
	
	@Override @Transactional
	public void codify(Collection<ScopeFunction> scopeFunctions) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("scopeFunctions", scopeFunctions);
		__codify__(scopeFunctions);
		updateMany(scopeFunctions);
	}
	
	@Override
	public void codifyByFunctions(Collection<Function> functions) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("functions", functions);
		Long t0 = System.currentTimeMillis();
		LogHelper.logInfo(String.format("Codification des postes des fonctions %s",functions), getClass());	
		Collection<ScopeTypeFunction> scopeTypeFunctions = ScopeTypeFunctionQuerier.getInstance().readByFunctionsIdentifiers(FieldHelper.readSystemIdentifiersAsStrings(functions));
		for(Function function : functions) {
			LogHelper.logInfo(String.format("\tTraitement de la fonction %s",function), getClass());
			/*
			// 1 - 
			LogHelper.logInfo(String.format("\tTraitement de tous les postes"), getClass());			
			if(StringHelper.isNotBlank(function.getCode()) || StringHelper.isNotBlank(function.getName())) {
				LogHelper.logInfo(String.format("\t\tChargement des %s",function.getCode()), getClass());
				Collection<ScopeFunction> scopeFunctions = ScopeFunctionQuerier.getInstance().readByFunctionsIdentifiers(List.of(function.getIdentifier()));
				LogHelper.logInfo(String.format("\t\t%s chargée(s)",CollectionHelper.getSize(scopeFunctions)), getClass());
				if(CollectionHelper.isEmpty(scopeFunctions))
					continue;
				LogHelper.logInfo(String.format("\t\tCodification"), getClass());
				
				Collection<ScopeType> scopeTypes = scopeFunctions.stream().map(scopeFunction -> scopeFunction.getScope().getType()).collect(Collectors.toSet());
				for(ScopeType scopeType : scopeTypes) {
					Collection<ScopeFunction> __scopeFunctions__ = scopeFunctions.stream().filter(scopeFunction -> scopeFunction.getScope().getType().equals(scopeType)).collect(Collectors.toList());
					if(CollectionHelper.isEmpty(__scopeFunctions__))
						continue;
					__codify__(scopeType.getCode(),__scopeFunctions__,function.getScopeFunctionCodeScript(),function.getScopeFunctionNameScript());
				}
				LogHelper.logInfo(String.format("\t\tEnregistrement",scopeFunctions.size()), getClass());
				EntityUpdater.getInstance().updateMany(CollectionHelper.cast(Object.class,scopeFunctions));
			}
			*/
			// 2 - 
			//LogHelper.logInfo(String.format("\tTraitement des postes par type de domaine"), getClass());
			Collection<ScopeTypeFunction> scopeTypeFunctionsOfFunction = scopeTypeFunctions.stream()
					.filter(scopeTypeFunction -> scopeTypeFunction.getFunction().equals(function)).collect(Collectors.toList()); 
			LogHelper.logInfo(String.format("\tNombre d'association(s) avec type de domaine : %s",scopeTypeFunctionsOfFunction.size()), getClass());
			if(CollectionHelper.isEmpty(scopeTypeFunctionsOfFunction))
				continue;
			for(ScopeTypeFunction scopeTypeFunction : scopeTypeFunctionsOfFunction) {
				LogHelper.logInfo(String.format("\t\tChargement des %s non codifié(e)s ayant %s",scopeTypeFunction.getScopeType().getCode(),scopeTypeFunction.getFunction().getCode()), getClass());
				Collection<ScopeFunction> scopeFunctions = ScopeFunctionQuerier.getInstance().readWhereCodificationDateIsNullByScopeTypesIdentifiersByFunctionsIdentifiers(List.of(scopeTypeFunction.getScopeType().getIdentifier()),List.of(scopeTypeFunction.getFunction().getIdentifier()));
				LogHelper.logInfo(String.format("\t\t%s chargée(s)",CollectionHelper.getSize(scopeFunctions)), getClass());
				if(CollectionHelper.isEmpty(scopeFunctions))
					continue;
				LogHelper.logInfo(String.format("\t\tCodification"), getClass());
				__codify__(scopeTypeFunction.getScopeType().getCode(),function.getCode(),null,scopeFunctions,scopeTypeFunction.getScopeFunctionCodeScript(),scopeTypeFunction.getScopeFunctionNameScript());
				LogHelper.logInfo(String.format("\t\tEnregistrement",scopeFunctions.size()), getClass());
				QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments();
				queryExecutorArguments.addObjects(CollectionHelper.cast(Object.class,scopeFunctions));
				queryExecutorArguments.setIsEntityManagerFlushable(Boolean.TRUE).setIsEntityManagerClearable(Boolean.TRUE).setIsEntityManagerClosable(Boolean.TRUE);
				EntityUpdater.getInstance().update(queryExecutorArguments);
			}
		}
		Long duration = System.currentTimeMillis() - t0;
		LogHelper.logInfo(String.format("Codification des postes terminée en %s", TimeHelper.formatDuration(duration)), getClass());
	}
	
	@Override
	public void codifyByFunctionsIdentifiers(Collection<String> functionsIdentifiers) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("functionsIdentifiers", functionsIdentifiers);
		codifyByFunctions(EntityFinder.getInstance().findMany(Function.class, functionsIdentifiers));
	}
	
	@Override
	public void codifyHoldersAndAssistantsByHoldersFunctionsIdentifiers(Collection<String> holdersFunctionsIdentifiers) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("holdersFunctionsIdentifiers", holdersFunctionsIdentifiers);
		codifyByFunctions(getHoldersAndAssistantsByHoldersFunctionsIdentifiers(holdersFunctionsIdentifiers));
	}
	
	@Override
	public void codifyAll() {
		Collection<Function> functions = FunctionQuerier.getInstance().readWhereAssociatedToScopeType();
		if(CollectionHelper.isEmpty(functions))
			return;
		codifyByFunctions(functions);
	}
	
	@Override
	@Transactional
	public void saveAssistants(Collection<ScopeFunction> scopeFunctions) {
		if(CollectionHelper.isEmpty(scopeFunctions))
			return;
		/*Collection<ScopeFunction> assistantsDb = ScopeFunctionQuerier.getInstance().readByParentsIdentifiers(FieldHelper.readSystemIdentifiersAsStrings(scopeFunctions));
		Collection<ScopeFunction> assistantsUser = new ArrayList<>();
		scopeFunctions.forEach(scopeFunction -> {
			
		});
		
		EntityManager entityManager = EntityManagerGetter.getInstance().get();
		for(ScopeFunction scopeFunction : scopeFunctions) {
			
		}*/
	}
	
	@Override
	public void deleteByFunctions(Collection<Function> functions) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("functions", functions);
		String queryString = String.format("DELETE FROM POSTE WHERE FONCTION IN (%s)",functions.stream().map(x -> PersistenceHelper.stringifyColumnValue(x.getIdentifier())).collect(Collectors.joining(",")));
		__inject__(NativeQueryStringExecutor.class).execute(new org.cyk.utility.persistence.query.NativeQueryStringExecutor.Arguments()
				.addQueriesStrings(queryString));
	}
	
	@Override
	public void deleteByFunctionsIdentifiers(Collection<String> functionsIdentifiers) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("functionsIdentifiers", functionsIdentifiers);
		deleteByFunctions(EntityFinder.getInstance().findMany(Function.class, functionsIdentifiers));
	}
	
	@Override
	public void deleteHoldersAndAssistantsByHoldersFunctionsIdentifiers(Collection<String> holdersFunctionsIdentifiers) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("holdersFunctionsIdentifiers", holdersFunctionsIdentifiers);
		deleteByFunctions(getHoldersAndAssistantsByHoldersFunctionsIdentifiers(holdersFunctionsIdentifiers));
	}

	@Override
	public BusinessEntity<ScopeFunction> deleteAll() {
		__inject__(NativeQueryStringExecutor.class).execute(new org.cyk.utility.persistence.query.NativeQueryStringExecutor.Arguments()
				.addQueriesStrings("DELETE FROM POSTE"));
		return this;
	}
	
	@Override @Transactional
	public void export(String actorCode) {
		actorCode = ValueHelper.defaultToIfBlank(actorCode, EntityLifeCycleListener.AbstractImpl.DEFAULT_USER_NAME);
		ScopeFunctionQuerier.getInstance().export(actorCode, "exportation", EntityLifeCycleListener.Event.UPDATE.getValue(), new Date(),EntityManagerGetter.getInstance().get());
	}
	
	@Override
	public void exportAsynchronously(String actorCode) {
		new Thread(new Runnable() {
			@Override
			public void run() {
				TimeHelper.pause(1000l * 5);
				try {
					export(actorCode);
				} catch (Exception exception) {
					LogHelper.log(exception, ScopeFunctionBusinessImpl.class);
				}
			}		
		}).start();
	}
	
	@Override
	public void updateActorsCodesFromExternal() {
		
	}
}