package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;
import java.util.ArrayList;

import org.cyk.utility.__kernel__.object.__static__.representation.AbstractIdentifiableSystemScalarStringAuditedImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class RequestScopeFunctionDto extends AbstractIdentifiableSystemScalarStringAuditedImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	private ArrayList<String> scopeFunctionsIdentifiers;
	
	@Override
	public RequestScopeFunctionDto setIdentifier(String identifier) {
		return (RequestScopeFunctionDto) super.setIdentifier(identifier);
	}

}