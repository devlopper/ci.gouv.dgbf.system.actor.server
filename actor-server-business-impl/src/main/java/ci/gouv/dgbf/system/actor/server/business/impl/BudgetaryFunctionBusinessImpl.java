package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import javax.enterprise.context.ApplicationScoped;
import javax.transaction.Transactional;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.log.LogHelper;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.BudgetaryFunctionBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.AdministrativeUnitPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.BudgetaryFunctionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.BudgetaryFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Section;

@ApplicationScoped
public class BudgetaryFunctionBusinessImpl extends AbstractBusinessEntityImpl<BudgetaryFunction, BudgetaryFunctionPersistence> implements BudgetaryFunctionBusiness,Serializable {
	private static final long serialVersionUID = 1L;

	@Override @Transactional
	public void generate() {
		LogHelper.logInfo(String.format("Generation des fonctions budgétaires en cours"), getClass());
		Long t0 = System.currentTimeMillis();
		Long count0 = __persistence__.count();
		Collection<BudgetaryFunction> budgetaryFunctions = new ArrayList<>();
		generateCreditManager(budgetaryFunctions);
		generateAuthorizingOfficer(budgetaryFunctions);
		generateFinancialController(budgetaryFunctions);
		generateAccounting(budgetaryFunctions);
		__persistence__.createMany(budgetaryFunctions);
		Long duration = System.currentTimeMillis() - t0;
		Long numberOfElements = __persistence__.count() - count0;
		LogHelper.logInfo(String.format("%s fonction(s) budgétaire(s) générée(s) en %s", numberOfElements,duration), getClass());
	}
	
	private void generate(Collection<BudgetaryFunction> budgetaryFunctions,Collection<?> objects) {
		if(CollectionHelper.isEmpty(objects))
			return;
		objects.forEach(object -> {
			budgetaryFunctions.add(new BudgetaryFunction().setCode(generateCode(object)).setName(generateName(object)));
		});
	}
	
	private void generateCreditManager(Collection<BudgetaryFunction> budgetaryFunctions) {
		Collection<AdministrativeUnit> administrativeUnits = __inject__(AdministrativeUnitPersistence.class).read();
		if(CollectionHelper.isEmpty(administrativeUnits))
			return;
		generate(budgetaryFunctions, administrativeUnits);
	}
	
	private void generateAuthorizingOfficer(Collection<BudgetaryFunction> budgetaryFunctions) {
		
	}
	
	private void generateFinancialController(Collection<BudgetaryFunction> budgetaryFunctions) {
		
	}
	
	private void generateAccounting(Collection<BudgetaryFunction> budgetaryFunctions) {
		
	}
	
	private String generateCode(Object object) {
		if(object == null)
			return null;
		String code = (String)FieldHelper.readBusinessIdentifier(object);
		if(StringHelper.isBlank(code))
			return null;
		if(object instanceof AdministrativeUnit)
			return "GC"+code;
		if(object instanceof Section)
			return "ORD"+code;
		throw new RuntimeException("La génération du code est impossible pour <<"+object+">>");
	}
	
	private String generateName(Object object) {
		if(object == null)
			return null;
		String name = (String)FieldHelper.readName(object);
		if(StringHelper.isBlank(name))
			return null;
		if(object instanceof AdministrativeUnit)
			return "Gestionnaire de crédits "+object;
		if(object instanceof Section)
			return "Ordonnateur principal "+object;
		throw new RuntimeException("La génération du libellé est impossible pour <<"+object+">>");
	}
}