package ci.gouv.dgbf.system.actor.server.business.api;

import java.util.Collection;

import javax.transaction.Transactional;

import org.cyk.utility.business.SpecificBusiness;
import org.cyk.utility.business.TransactionResult;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;

public interface ScopeTypeBusiness extends SpecificBusiness<ScopeType> {

	String CREATE = "ScopeType.create";
	@Transactional
	TransactionResult create(String code,String name,Byte orderNumber,Boolean requestable,String actorCode);
	
	String UPDATE = "ScopeType.update";
	@Transactional
	TransactionResult update(String identifier,String code,String name,Byte orderNumber,Boolean requestable,String actorCode);
	
	String SAVE = "ScopeType.save";
	@Transactional
	TransactionResult save(String identifier,String code,String name,Byte orderNumber,Boolean requestable,String actorCode);
	
	Collection<ScopeType> get(Boolean requestable,Boolean pageable,Integer firstTupleIndex,Integer numberOfTuples);
}