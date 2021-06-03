package ci.gouv.dgbf.system.actor.server.business.api;

import java.util.Collection;

import javax.transaction.Transactional;

import org.cyk.utility.business.TransactionResult;
import org.cyk.utility.persistence.query.Filter;
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
	@Transactional
	void clean(String actorCode);
	
	String IMPORT = "Assignments.import";
	@Transactional
	void import_(String actorCode);
	
	String IMPORT_NEWS = "Assignments.importNews";
	void importNews(String actorCode);
	
	String EXPORT = "Assignments.export";
	void export(String actorCode);
}