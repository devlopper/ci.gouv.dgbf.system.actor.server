package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;
import java.util.List;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.__kernel__.properties.Properties;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.persistence.query.EntityFinder;
import org.cyk.utility.persistence.server.query.executor.field.CodeExecutor;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

import ci.gouv.dgbf.system.actor.server.persistence.api.ScopeFunctionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeTypeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.BudgetSpecializationUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Locality;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeTypeFunction;

@ApplicationScoped
public class ScopeFunctionPersistenceImpl extends AbstractPersistenceEntityImpl<ScopeFunction> implements ScopeFunctionPersistence,Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override
	public String computeCreditManagerHolderName(AdministrativeUnit administrativeUnit) {
		if(administrativeUnit == null)
			return null;
		ScopeTypeFunction scopeTypeFunction = ScopeTypeFunctionQuerier.getInstance().readByScopeTypeCodeByFunctionCode(ScopeType.CODE_UA, Function.CODE_CREDIT_MANAGER_HOLDER);
		ScopeFunction scopeFunction = new ScopeFunction()
				.setScope(EntityFinder.getInstance().find(Scope.class,administrativeUnit.getIdentifier()))
				.setFunction(EntityFinder.getInstance().find(Function.class,Function.CODE_CREDIT_MANAGER_HOLDER));
		ScopeFunction.computeCodeAndName(ScopeType.CODE_UA, List.of(scopeFunction), 0, 0, scopeTypeFunction.getScopeFunctionCodeScript()
				, scopeTypeFunction.getScopeFunctionNameScript());
		return scopeFunction.getName();
	}
	
	@Override
	public String computeCreditManagerHolderName(String administrativeUnitIdentifier) {
		if(StringHelper.isBlank(administrativeUnitIdentifier))
			return null;
		return computeCreditManagerHolderName(EntityFinder.getInstance().find(AdministrativeUnit.class, administrativeUnitIdentifier));
	}
	
	@Override
	public String computeAuthorizingOfficerHolderName(BudgetSpecializationUnit budgetSpecialisationUnit,Locality locality) {
		if(budgetSpecialisationUnit == null)
			return null;
		ScopeTypeFunction scopeTypeFunction = ScopeTypeFunctionQuerier.getInstance().readByScopeTypeCodeByFunctionCode(ScopeType.CODE_SERVICE_ORD, Function.CODE_AUTHORIZING_OFFICER_HOLDER);
		ScopeFunction scopeFunction = new ScopeFunction()
				.setScope(EntityFinder.getInstance().find(Scope.class,budgetSpecialisationUnit.getIdentifier()))
				.setFunction(CodeExecutor.getInstance().getOne(Function.class,Function.CODE_AUTHORIZING_OFFICER_HOLDER))
				.setLocality(locality);
		ScopeFunction.computeCodeAndName(ScopeType.CODE_SERVICE_ORD, List.of(scopeFunction), 0, 0, scopeTypeFunction.getScopeFunctionCodeScript()
				, scopeTypeFunction.getScopeFunctionNameScript());
		return scopeFunction.getName();
	}
	
	@Override
	public String computeAuthorizingOfficerHolderName(String budgetSpecialisationUnitIdentifier,String localityIdentifier) {
		if(StringHelper.isBlank(budgetSpecialisationUnitIdentifier))
			return null;
		return computeAuthorizingOfficerHolderName(EntityFinder.getInstance().find(BudgetSpecializationUnit.class, budgetSpecialisationUnitIdentifier)
				,StringHelper.isBlank(localityIdentifier) ? null : EntityFinder.getInstance().find(Locality.class, localityIdentifier));
	}
	
	@Override
	protected void __listenExecuteReadAfter__(ScopeFunction scopeFunction, Properties properties) {
		super.__listenExecuteReadAfter__(scopeFunction, properties);
		scopeFunction.setShared(ScopeFunctionPersistence.computeShared(scopeFunction.getNumberOfActor()));
		scopeFunction.set__auditWhen__(null);
	}
	
	@Override
	public ScopeFunction readBySystemIdentifier(Object identifier, Properties properties) {
		ScopeFunction scopeFunction = super.readBySystemIdentifier(identifier, properties);
		if(scopeFunction != null)
			scopeFunction.set__auditWhen__(null);
		return scopeFunction;
	}
}