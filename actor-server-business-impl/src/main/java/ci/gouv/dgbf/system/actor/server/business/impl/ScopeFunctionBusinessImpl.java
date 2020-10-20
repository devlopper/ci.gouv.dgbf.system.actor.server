package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import javax.enterprise.context.ApplicationScoped;
import javax.transaction.Transactional;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.log.LogHelper;
import org.cyk.utility.__kernel__.properties.Properties;
import org.cyk.utility.__kernel__.throwable.ThrowableHelper;
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
	
	@Override @Transactional
	public void deriveFromScopesFromFunctions(Collection<Scope> scopes, Collection<Function> functions) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("scopes", scopes);
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("functions", functions);
		LogHelper.logInfo(String.format("Création des postes des fonctions "+functions+" en cours"), getClass());
		Long t0 = System.currentTimeMillis();
		Long count0 = __persistence__.count();
		Collection<ScopeFunction> existingScopeFunctions = ScopeFunctionQuerier.getInstance().readWithCodesOnlyByFunctionsIdentifiers(FieldHelper.readSystemIdentifiersAsStrings(functions));
		LogHelper.logInfo(String.format(CollectionHelper.getSize(existingScopeFunctions)+" poste(s) existante(s)"), getClass());
		Collection<ScopeFunction> scopeFunctions = new ArrayList<>();
		scopes.forEach(scope -> {
			functions.forEach(function -> {
				if(!Boolean.TRUE.equals(ScopeFunction.isExistsByCodesOnly(existingScopeFunctions, scope, function))) {
					scopeFunctions.add(new ScopeFunction().setScope(scope).setFunction(function));
				}
			});
		});
		LogHelper.logInfo(String.format(CollectionHelper.getSize(scopeFunctions)+" poste(s) à créer"), getClass());
		if(CollectionHelper.isNotEmpty(scopeFunctions)) {
			__codify__(scopeFunctions);
			scopeFunctions.forEach(scopeFunction -> {
				scopeFunction.setIdentifier(scopeFunction.getCode());
			});
			createMany(scopeFunctions);
		}		
		Long duration = System.currentTimeMillis() - t0;
		Long numberOfElements = __persistence__.count() - count0;
		LogHelper.logInfo(String.format("%s poste(s) créé(s) en %s", numberOfElements,duration), getClass());
	}

	@Override
	public void deriveAll() {
		LogHelper.logInfo(String.format("Dérivation de tous les postes en cours"), getClass());
		Long t0 = System.currentTimeMillis();
		Collection<ScopeTypeFunction> scopeTypeFunctions = ScopeTypeFunctionQuerier.getInstance().readWhereScopeFunctionDerivableIsTrue();
		LogHelper.logInfo(String.format(CollectionHelper.getSize(scopeTypeFunctions)+" association(s) de type de domaine et de fonction avec l'option dérivable à vrai trouvée(s)"), getClass());
		if(CollectionHelper.isEmpty(scopeTypeFunctions))
			return;
		Collection<ScopeType> scopeTypes = scopeTypeFunctions.stream().map(x -> x.getScopeType()).collect(Collectors.toSet());
		if(CollectionHelper.isEmpty(scopeTypes))
			return;
		LogHelper.logInfo(String.format(CollectionHelper.getSize(scopeTypes)+" type(s) de domaine à traiter : "+scopeTypes), getClass());
		for(ScopeType scopeType : scopeTypes) {
			Collection<Scope> scopes = ScopeQuerier.getInstance().readByTypesCodes(List.of(scopeType.getCode()));
			if(CollectionHelper.isEmpty(scopes))
				continue;
			Collection<Function> functions = scopeTypeFunctions.stream().filter(x -> x.getScopeType().equals(scopeType)).map(x -> x.getFunction()).collect(Collectors.toSet());
			if(CollectionHelper.isEmpty(functions))
				continue;
			deriveFromScopesFromFunctions(scopes, functions);
		}
		Long duration = System.currentTimeMillis() - t0;
		LogHelper.logInfo(String.format("Dérivation de tous les postes en terminé en %s",duration), getClass());
	}
	
	private void __codify__(Collection<ScopeFunction> scopeFunctions) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("postes", scopeFunctions);
		//Long t0 = System.currentTimeMillis();
		//LogHelper.logInfo(String.format("Codification de "+CollectionHelper.getSize(scopeFunctions)+" poste(s) en cours"), getClass());
		scopeFunctions.forEach(scopeFunction -> {
			Scope scope = scopeFunction.getScope();
			Function function = scopeFunction.getFunction();
			scopeFunction.setCode(generateCode(scope,function)).setName(generateName(scope,function));
		});
		//Long duration = System.currentTimeMillis() - t0;
		//LogHelper.logInfo(String.format("%s poste(s) codifié(s) en %s", CollectionHelper.getSize(scopeFunctions),duration), getClass());
	}
	
	@Override @Transactional
	public void codify(Collection<ScopeFunction> scopeFunctions) {
		__codify__(scopeFunctions);
		updateMany(scopeFunctions);
	}
	
	@Override
	public void codifyAll() {
		LogHelper.logInfo(String.format("Codification de tous les postes en cours"), getClass());
		Long t0 = System.currentTimeMillis();
		Collection<ScopeFunction> scopeFunctions = ScopeFunctionQuerier.getInstance().read();
		if(CollectionHelper.isNotEmpty(scopeFunctions)) {
			codify(scopeFunctions);
		}		
		Long duration = System.currentTimeMillis() - t0;
		LogHelper.logInfo(String.format("Codification de tous les postes terminé en %s", duration), getClass());
	}
	
	private String generateCode(Scope scope,Function function) {
		if(scope == null || function == null)
			return null;
		return function.getCode()+scope.getCode();
	}
	
	private String generateName(Scope scope,Function function) {
		if(scope == null || function == null)
			return null;
		return function.getName()+" "+scope.getName();
	}
}