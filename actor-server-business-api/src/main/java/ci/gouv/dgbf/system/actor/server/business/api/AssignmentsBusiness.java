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
	
	TransactionResult initialize();
	
	@Transactional
	TransactionResult save(Collection<Assignments> collection);
	
	TransactionResult applyModel(Assignments model,Filter filter,Collection<String> overridablesFieldsNames);
	
	String INITIALIZE = "Assignments.initialize";
	String INITIALIZE_MANY = "Assignments.initializeMany";
	
	String SAVE = "Assignments.save";	
	
	String APPLY_MODEL = "Assignments.applyModel";
}
