package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;

import org.cyk.utility.__kernel__.object.__static__.representation.AbstractIdentifiableSystemScalarStringImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class AccountRequestFunctionDto extends AbstractIdentifiableSystemScalarStringImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	private AccountRequestDto accountRequest;
	private FunctionDto function;
	
	@Override
	public AccountRequestFunctionDto setIdentifier(String identifier) {
		return (AccountRequestFunctionDto) super.setIdentifier(identifier);
	}

}