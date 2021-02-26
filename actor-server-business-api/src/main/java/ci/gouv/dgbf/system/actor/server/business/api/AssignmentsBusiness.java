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
	TransactionResult initialize(String actorCode);
	
	String INITIALIZE_MANY = "Assignments.initializeMany";
	
	String DERIVE_ALL_VALUES = "Assignments.deriveAllValues";
	TransactionResult deriveAllValues(Boolean holdersSettable,Boolean assistantsSettable,Boolean overridable,String actorCode);
	
	String DERIVE_VALUES = "Assignments.deriveValues";
	@Transactional
	TransactionResult deriveValues(Collection<Assignments> collection,Boolean holdersSettable,Boolean assistantsSettable,Boolean overridable,String actorCode);
	
	String SAVE_SCOPE_FUNCTIONS = "Assignments.saveScopeFunctions";
	@Transactional
	TransactionResult saveScopeFunctions(Collection<Assignments> collection);
	
	String APPLY_MODEL = "Assignments.applyModel";
	TransactionResult applyModel(Assignments model,Filter filter,Collection<String> overridablesFieldsNames,String actorCode);
	
	String DELETE_ALL = "Assignments.deleteAll";
	
	String CLEAN = "Assignments.clean";
	void clean(String actorCode);
	
	String IMPORT = "Assignments.import";
	void import_(String actorCode);
	
	String EXPORT = "Assignments.export";
	@Transactional
	//FIXME @Transactional is not required - @Transactional is used as a workaround to release connection and avoid connection leak.
	//user utility-persistence-server-hibernate to solve this
	void export(String actorCode);
}