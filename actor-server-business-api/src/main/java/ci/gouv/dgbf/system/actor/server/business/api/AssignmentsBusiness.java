package ci.gouv.dgbf.system.actor.server.business.api;

import java.util.Collection;

import javax.transaction.Transactional;

import org.cyk.utility.__kernel__.business.TransactionResult;
import org.cyk.utility.__kernel__.persistence.query.filter.Filter;
import org.cyk.utility.server.business.BusinessEntity;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;

public interface AssignmentsBusiness extends BusinessEntity<Assignments> {

	//@Transactional
	//void deriveFromExecutionImputations(Collection<Assignments> assignments);
	
	String INITIALIZE = "Assignments.initialize";
	TransactionResult initialize();
	
	String INITIALIZE_MANY = "Assignments.initializeMany";
	
	String DERIVE_ALL_VALUES = "Assignments.deriveAllValues";
	TransactionResult deriveAllValues(Boolean overridable);
	
	String DERIVE_VALUES = "Assignments.deriveValues";
	@Transactional
	TransactionResult deriveValues(Collection<Assignments> collection,Boolean overridable);
	
	String SAVE_SCOPE_FUNCTIONS = "Assignments.saveScopeFunctions";
	@Transactional
	TransactionResult saveScopeFunctions(Collection<Assignments> collection);
	
	String APPLY_MODEL = "Assignments.applyModel";
	TransactionResult applyModel(Assignments model,Filter filter,Collection<String> overridablesFieldsNames);
	
	String DELETE_ALL = "Assignments.deleteAll";
}