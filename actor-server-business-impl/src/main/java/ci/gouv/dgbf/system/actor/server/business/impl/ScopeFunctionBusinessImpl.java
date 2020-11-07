package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import javax.enterprise.context.ApplicationScoped;
import javax.transaction.Transactional;

import org.cyk.utility.__kernel__.business.EntityCreator;
import org.cyk.utility.__kernel__.business.EntityUpdater;
import org.cyk.utility.__kernel__.business.NativeQueryStringExecutor;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.log.LogHelper;
import org.cyk.utility.__kernel__.persistence.PersistenceHelper;
import org.cyk.utility.__kernel__.persistence.query.EntityFinder;
import org.cyk.utility.__kernel__.properties.Properties;
import org.cyk.utility.__kernel__.runnable.Executor;
import org.cyk.utility.__kernel__.throwable.ThrowableHelper;
import org.cyk.utility.__kernel__.time.TimeHelper;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;
import org.cyk.utility.server.business.BusinessEntity;
import org.cyk.utility.server.business.BusinessFunctionCreator;
import org.cyk.utility.server.business.BusinessFunctionModifier;

import ci.gouv.dgbf.system.actor.server.business.api.ScopeFunctionBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ScopeFunctionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ExecutionImputationQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.FunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeTypeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeTypeFunction;

@ApplicationScoped
public class ScopeFunctionBusinessImpl extends AbstractBusinessEntityImpl<ScopeFunction, ScopeFunctionPersistence> implements ScopeFunctionBusiness,Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override
	protected void __listenExecuteCreateBefore__(ScopeFunction scopeFunction, Properties properties,BusinessFunctionCreator function) {
		super.__listenExecuteCreateBefore__(scopeFunction, properties, function);
		__listenExecuteCreateOrUpdateBefore__(scopeFunction);		
	}
	
	@Override
	protected void __listenExecuteUpdateBefore__(ScopeFunction scopeFunction, Properties properties,BusinessFunctionModifier function) {
		super.__listenExecuteUpdateBefore__(scopeFunction, properties, function);
		__listenExecuteCreateOrUpdateBefore__(scopeFunction);
	}
	
	private void __listenExecuteCreateOrUpdateBefore__(ScopeFunction scopeFunction) {
		scopeFunction.setNumberOfActor(ScopeFunctionPersistence.computeNumberOfActor(scopeFunction.getShared()));
		__codify__(List.of(scopeFunction));
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
				Collection<ScopeFunction> scopeFunctions = new ArrayList<>();
				for(Scope scope : scopes) {
					ScopeFunction scopeFunction = new ScopeFunction().setScope(scope).setFunction(function).setNumberOfActor(function.getNumberOfActorPerScope());
					scopeFunctions.add(scopeFunction);
				}
				LogHelper.logInfo(String.format("\t\t%s poste(s) instantié(s)",CollectionHelper.getSize(scopeFunctions)), getClass());
				if(CollectionHelper.isEmpty(scopeFunctions))
					return;
				LogHelper.logInfo(String.format("\t\tCodification des postes",scopeFunctions.size()), getClass());
				__codify__(scopeTypeFunction.getScopeType().getCode(),scopeFunctions,scopeTypeFunction.getScopeFunctionCodeScript(),scopeTypeFunction.getScopeFunctionNameScript());
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
	
	private void __codify__(String scopeTypeCode,Collection<ScopeFunction> scopeFunctions,String codeScript,String nameScript) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("scopeFunctions", scopeFunctions);
		ScopeFunction.computeCodeAndName(scopeTypeCode,scopeFunctions, codeScript, nameScript);
		/*
		Executor<Runnable> executor = new Executor<Runnable>().setName("Générateur des codes et des libellés").setNumberOfRunnablesToBeExecuted(scopeFunctions.size())
				//.setExecutorService(RunnableHelper.instantiateExecutorService(1, 100, 3l, TimeUnit.MINUTES, null, scopeFunctions.size(), null, null))
				;
		//executor.start();		
		scopeFunctions.forEach(scopeFunction -> {
			executor.addRunnables(new Runnable() {
				@Override
				public void run() {
					scopeFunction.computeAndSetCode(
							codeScript, scopeFunction.getScope().getType().getCode(), scopeFunction.getScope().getCode(), scopeFunction.getFunction().getCode());
					scopeFunction.computeAndSetName(
							nameScript, scopeFunction.getScope().getType().getCode(), scopeFunction.getScope().getName(), scopeFunction.getFunction().getName());
				}			
			});
		});		
		//executor.join();
		executor.run();
		*/
	}
	
	private void __codify__(Collection<ScopeFunction> scopeFunctions) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("scopeFunctions", scopeFunctions);
		Collection<ScopeTypeFunction> scopeTypeFunctions = ScopeTypeFunctionQuerier.getInstance().read();		
		scopeFunctions.parallelStream().forEach(scopeFunction -> {
			scopeFunction.setScopeTypeFunction(CollectionHelper.getFirst(scopeTypeFunctions.stream()
					.filter(index -> index.getScopeType().equals(scopeFunction.getScope().getType()) 
					&& index.getFunction().equals(scopeFunction.getFunction())).collect(Collectors.toList())));
		});		
		Executor<Runnable> executor = new Executor<Runnable>().setName("Générateur des codes et des libellés").setNumberOfRunnablesToBeExecuted(scopeFunctions.size())
				//.setExecutorService(RunnableHelper.instantiateExecutorService(1, 100, 3l, TimeUnit.MINUTES, null, scopeFunctions.size(), null, null))
				;
		//executor.start();		
		scopeFunctions.forEach(scopeFunction -> {
			executor.addRunnables(new Runnable() {
				@Override
				public void run() {
					scopeFunction.computeAndSetCode(
							scopeFunction.getScopeTypeFunction() == null ? null : scopeFunction.getScopeTypeFunction().getScopeFunctionCodeScript()
							, scopeFunction.getScopeTypeFunction() == null ? null : scopeFunction.getScope().getType().getCode()
							, scopeFunction.getScope().getCode(), scopeFunction.getFunction().getCode());
					scopeFunction.computeAndSetName(
							scopeFunction.getScopeTypeFunction() == null ? null : scopeFunction.getScopeTypeFunction().getScopeFunctionNameScript()
							, scopeFunction.getScopeTypeFunction() == null ? null : scopeFunction.getScope().getType().getCode()
							, scopeFunction.getScope().getName(), scopeFunction.getFunction().getName());
				}			
			});
		});		
		//executor.join();
		executor.run();
	}
	
	@Override @Transactional
	public void codify(Collection<ScopeFunction> scopeFunctions) {
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
			Collection<ScopeTypeFunction> scopeTypeFunctionsOfFunction = scopeTypeFunctions.stream()
					.filter(scopeTypeFunction -> scopeTypeFunction.getFunction().equals(function)).collect(Collectors.toList()); 
			LogHelper.logInfo(String.format("\tNombre d'association(s) avec type de domaine : %s",scopeTypeFunctionsOfFunction.size()), getClass());
			if(CollectionHelper.isEmpty(scopeTypeFunctionsOfFunction))
				continue;
			for(ScopeTypeFunction scopeTypeFunction : scopeTypeFunctionsOfFunction) {
				LogHelper.logInfo(String.format("\t\tChargement des %s ayant %s",scopeTypeFunction.getScopeType().getCode(),scopeTypeFunction.getFunction().getCode()), getClass());
				Collection<ScopeFunction> scopeFunctions = ScopeFunctionQuerier.getInstance().readByScopeTypesIdentifiersByFunctionsIdentifiers(List.of(scopeTypeFunction.getScopeType().getIdentifier()),List.of(scopeTypeFunction.getFunction().getIdentifier()));
				LogHelper.logInfo(String.format("\t\t%s chargée(s)",CollectionHelper.getSize(scopeFunctions)), getClass());
				if(CollectionHelper.isEmpty(scopeFunctions))
					continue;
				LogHelper.logInfo(String.format("\t\tCodification"), getClass());
				__codify__(scopeTypeFunction.getScopeType().getCode(),scopeFunctions,scopeTypeFunction.getScopeFunctionCodeScript(),scopeTypeFunction.getScopeFunctionNameScript());
				LogHelper.logInfo(String.format("\t\tEnregistrement",scopeFunctions.size()), getClass());
				EntityUpdater.getInstance().updateMany(CollectionHelper.cast(Object.class,scopeFunctions));
			}
		}
		ExecutionImputationQuerier.refreshMaterializedView();
		Long duration = System.currentTimeMillis() - t0;
		LogHelper.logInfo(String.format("Codification des postes terminée en %s", TimeHelper.formatDuration(duration)), getClass());
	}
	
	@Override
	public void codifyByFunctionsIdentifiers(Collection<String> functionsIdentifiers) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("functionsIdentifiers", functionsIdentifiers);
		codifyByFunctions(EntityFinder.getInstance().findMany(Function.class, functionsIdentifiers));
	}
	
	@Override
	public void codifyAll() {
		Collection<Function> functions = FunctionQuerier.getInstance().readWhereAssociatedToScopeType();
		if(CollectionHelper.isEmpty(functions))
			return;
		codifyByFunctions(functions);
	}
	
	@Override
	public void deleteByFunctions(Collection<Function> functions) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("functions", functions);
		String queryString = String.format("DELETE FROM POSTE WHERE FONCTION IN (%s)",functions.stream().map(x -> PersistenceHelper.stringifyColumnValue(x.getIdentifier())).collect(Collectors.joining(",")));
		__inject__(NativeQueryStringExecutor.class).execute(new org.cyk.utility.__kernel__.persistence.query.NativeQueryStringExecutor.Arguments()
				.addQueriesStrings(queryString));
	}
	
	@Override
	public void deleteByFunctionsIdentifiers(Collection<String> functionsIdentifiers) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("functionsIdentifiers", functionsIdentifiers);
		deleteByFunctions(EntityFinder.getInstance().findMany(Function.class, functionsIdentifiers));
	}

	@Override
	public BusinessEntity<ScopeFunction> deleteAll() {
		__inject__(NativeQueryStringExecutor.class).execute(new org.cyk.utility.__kernel__.persistence.query.NativeQueryStringExecutor.Arguments()
				.addQueriesStrings("DELETE FROM POSTE"));
		return this;
	}
}