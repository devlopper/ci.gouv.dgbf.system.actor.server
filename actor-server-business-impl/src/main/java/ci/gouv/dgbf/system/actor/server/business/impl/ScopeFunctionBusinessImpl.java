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
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.log.LogHelper;
import org.cyk.utility.__kernel__.number.NumberHelper;
import org.cyk.utility.__kernel__.properties.Properties;
import org.cyk.utility.__kernel__.runnable.Executor;
import org.cyk.utility.__kernel__.throwable.ThrowableHelper;
import org.cyk.utility.__kernel__.time.TimeHelper;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;
import org.cyk.utility.server.business.BusinessFunctionCreator;
import org.cyk.utility.server.business.BusinessFunctionModifier;

import ci.gouv.dgbf.system.actor.server.business.api.ScopeFunctionBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ScopeFunctionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeTypeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;
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
	public void deriveByFunctionsIdentifiers(Collection<String> functionsIdentifiers) {
		// TODO Auto-generated method stub
		
	}

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
	public void codifyByFunctionsIdentifiers(Collection<String> functionsIdentifiers) {
		// TODO Auto-generated method stub
		
	}
	
	@Override
	public void codifyAll() {
		LogHelper.logInfo(String.format("Codification de tous les postes"), getClass());
		Long count = ScopeFunctionQuerier.getInstance().count();
		LogHelper.logInfo(String.format("%s poste(s) trouvé(s)",count), getClass());
		if(NumberHelper.isEqualToZero(count))
			return;
		Long t0 = System.currentTimeMillis();
		LogHelper.logInfo(String.format("Chargement des postes en cours..."), getClass());
		Collection<ScopeFunction> scopeFunctions = ScopeFunctionQuerier.getInstance().read();
		LogHelper.logInfo(String.format("%s poste(s) chargé(s)",CollectionHelper.getSize(scopeFunctions)), getClass());
		if(CollectionHelper.isEmpty(scopeFunctions))
			return;
		__codify__(scopeFunctions);
		EntityUpdater.getInstance().updateMany(CollectionHelper.cast(Object.class,scopeFunctions));
		Long duration = System.currentTimeMillis() - t0;
		LogHelper.logInfo(String.format("Codification de tous les postes terminé en %s", TimeHelper.formatDuration(duration)), getClass());
	}
	
	@Override
	public void deleteByFunctionsIdentifiers(Collection<String> functionsIdentifiers) {
		
	}
}